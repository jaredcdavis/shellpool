; Shellpool - Interface from Common Lisp to External Programs
; Copyright (C) 2014-2015 Kookamara LLC
;
; Contact:
;
;   Kookamara LLC
;   11410 Windermere Meadows
;   Austin, TX 78759, USA
;   http://www.kookamara.com/
;
; License: (An MIT/X11-style license)
;
;   Permission is hereby granted, free of charge, to any person obtaining a
;   copy of this software and associated documentation files (the "Software"),
;   to deal in the Software without restriction, including without limitation
;   the rights to use, copy, modify, merge, publish, distribute, sublicense,
;   and/or sell copies of the Software, and to permit persons to whom the
;   Software is furnished to do so, subject to the following conditions:
;
;   The above copyright notice and this permission notice shall be included in
;   all copies or substantial portions of the Software.
;
;   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;   FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;   DEALINGS IN THE SOFTWARE.
;
; Original author: Jared Davis <jared@kookamara.com>

; test-kill.lsp -- tests of killing shellpool processes

(ql:quickload "shellpool")
(shellpool:start 2)
(setq shellpool:*debug* nil)

; So far we have tested most of the single-threading stuff.  We're now going to
; do some multi-threaded tests and try to make sure our kill mechanisms are
; working correctly.

(defun ezrun (cmd)
  (let* ((stdout nil)
         (stderr nil)
         (each-line (lambda (line type)
                      (case type
                        (:stdout (push line stdout))
                        (:stderr (push line stderr))
                        (otherwise (error "Bad type ~s for line ~s~%" type line)))))
         (status (shellpool:run cmd :each-line each-line))
         (stdout (nreverse stdout))
         (stderr (nreverse stderr)))
    (when stderr
      (error "Error running ~s: Got lines on stderr: ~s" cmd stderr))
    (when (not (equal status 0))
      (error "Error running ~s: non-zero exit status ~s" cmd status))
    stdout))

(defun list-processes ()
  ;; See if a program is running.
  ;; BOZO don't know how portable this is.
  (ezrun "ps ax"))

(defun has-process (name)
  (let ((all-processes (list-processes)))
    (loop for entry in all-processes do
          (when (shellpool::strpos name entry)
            (return-from has-process t)))
    nil))

(unless (has-process "bash")
  (error "Doesn't seem like has-process is working right: no bash?"))

(when (has-process "lollipops-dancing-on-my-elbows.exe")
  (error "Doesn't seem like has-process is working right: unlikely process exists."))

(when (has-process "sleep.pl")
  (error "Looks like sleep.pl is running already, won't be able to test killing correctly."))



(let ((sem   (bt-sem:make-semaphore))
      (lock  (bt:make-lock))
      (queue nil))

  (defun msg (msg &rest args)
    (bt:with-lock-held (lock)
                       (push (cons msg args) queue))
    (bt-sem:signal-semaphore sem))

  (defun print-thread ()
    (loop do
          (unless (bt-sem:wait-on-semaphore sem)
            (error "Failed to get the print semaphore."))
          (let ((pair nil))
            (bt:with-lock-held (lock)
                               (setq pair (pop queue)))
            (let ((msg (car pair))
                  (args (cdr pair)))
              (eval `(format t ,msg . ,args))
              (force-output)))))

  (bt:make-thread 'print-thread))

(msg "this is a test, one is ~s and two is ~s.~%" 1 2)



(defparameter *test-lock* (bt:make-lock))
(defparameter *test-started* nil)  ;; Set on first output message
(defparameter *done-running* nil)  ;; Set by unwind-protect whenever the command exits

(defun killtest-thread ()
  (msg "Starting sleep thread.~%")
  (let* ((each-line (lambda (line type)
                      (declare (ignore type))
                      (bt:with-lock-held (*test-lock*)
                                         (setq *test-started* t))
                      (msg "KT ~s~%" line)))

         (result (catch 'shellpool-test-tag
                   (unwind-protect
                       (shellpool:run "./sleep.pl 10" :each-line each-line)
                     (progn
                       (msg "Setting done-running to true.~%")
                       (setq *done-running* t))))))
    (msg "Killtest thread got result: ~s~%" result)))

(defun wait-until-started ()
  (msg "Waiting for the start signal.~%")
  (loop do
        (bt:with-lock-held (*test-lock*)
                           (msg "Got started signal.~%")
                           (when *test-started*
                             (return-from wait-until-started nil)))))

(defun wait-until-done ()
  (msg "Waiting for done signal.~%")
  (loop do
        (bt:with-lock-held (*test-lock*)
                           (when *done-running*
                             (msg "Got done signal.~%")
                             (return-from wait-until-done nil)))))

(defun do-test ()
  (let ((other-thread (bt:make-thread 'killtest-thread))
        (start-time   (get-internal-run-time)))
    (wait-until-started)
    (unless (has-process "sleep.pl")
      (error "Doesn't seem like sleep.pl got started."))
    (bt:interrupt-thread other-thread
                         (lambda ()
                           (msg "Interrupting runner thread.~%")
                           (throw 'shellpool-test-tag "Interrupt from do-test")))
    (wait-until-done)
    (when (has-process "sleep.pl")
      (error "Doesn't seem like sleep.pl got killed."))
    (let* ((end-time (get-internal-run-time))
           (elapsed  (- end-time start-time))
           (limit    (* 5.0 internal-time-units-per-second)))
      (unless (< elapsed limit)
        (error "Seems like that took too long.")))))

(time (do-test))


;; Cool.  so, for next time:
;;
;;   - turn this into a reusable test harness.
;;
;;   - write a troublesome program that ignores sighup/kill etc and
;;     figure out how to get it working.
;;
;;   - write a program that spawns children in the background and
;;     make sure they get killed.  also make sure they get killed
;;     even if they are troublesome children.
;;
;;   - do we need all this pgrep crap or can we just do it in bash?
