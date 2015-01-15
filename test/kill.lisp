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

; kill.lisp -- tests of killing shellpool processes

(let ((lock    (bt:make-lock))  ;; Protects started/done
      (started nil)             ;; Set on first output message
      (done    nil)             ;; Set by unwind-protect whenever the command exits
      (runner  nil)             ;; The thread that will run the test
      )

  (defun interrupt-runner ()
    (msg "Sending interrupt to runner thread.~%")
    (bt:interrupt-thread runner
                         (lambda ()
                           (msg "In runner thread, now doing interrupt.~%")
                           (throw 'shellpool-test-tag "Interrupt from do-test"))))

  (defun start-runner ()
    (msg "Starting runner thread.~%")
    (let* ((each-line (lambda (line type)
                        (declare (ignore type))
                        (bt:with-lock-held (lock)
                                           (setq started t))
                        (msg "KT ~s~%" line)))

           (result (catch 'shellpool-test-tag
                     (unwind-protect
                         (shellpool:run "./sleep.pl 10" :each-line each-line)
                       (progn
                         (msg "Setting done to true.~%")
                         (setq done t))))))
    (msg "Killtest thread got result: ~s~%" result)))

  (defun wait-until-started ()
    (msg "Waiting for the start signal.~%")
    (loop do
          (bt:with-lock-held (lock)
                             (when started
                               (msg "Got started signal.~%")
                               (return-from wait-until-started nil)))))

  (defun wait-until-done ()
    (msg "Waiting for done signal.~%")
    (loop do
          (bt:with-lock-held (lock)
                             (when done
                               (msg "Got done signal.~%")
                               (return-from wait-until-done nil)))))

  (defun do-test ()
    (when (has-process "sleep.pl")
      (error "Looks like sleep.pl is running already, won't be able to test killing correctly."))
    (let ((start-time (get-internal-run-time)))
      ;; In the runner thread, start sleeping for 10 seconds.
      (setq runner (bt:make-thread 'start-runner))
      ;; Wait for the first line of output from the command (which it prints
      ;; right away when it starts running.)
      (wait-until-started)
      ;; Now that we've got the first line of output, we're sure the command
      ;; should be running.  So let's try to verify that it is indeed running.
      (unless (has-process "sleep.pl")
        (error "Doesn't seem like sleep.pl got started."))
      ;; Since it is running, let's now try to interrupt it.
      (interrupt-runner)
      ;; Wait till the DONE flag gets set.  Hopefully it is set because the
      ;; command was killed by our interrupt.
      (wait-until-done)
      ;; Try to verify that the process indeed got killed as desired.
      (when (has-process "sleep.pl")
        (error "Doesn't seem like sleep.pl got killed."))
      ;; Try to verify that all of the above happened very fast, i.e., we
      ;; didn't just sit around waiting for the command to exit.  On an AMD
      ;; FX-8350 Linux/CCL, I measured this at 0.09 seconds.  I'll just assume
      ;; that anything under 5 seconds is fine.  (After all, if we really
      ;; fail to interrupt, it should take 10 seconds.)
      (let* ((end-time (get-internal-run-time))
             (elapsed  (- end-time start-time))
             (limit    (* 5.0 internal-time-units-per-second)))
        (unless (< elapsed limit)
          (error "Seems like that took too long."))))))

(shellpool:start) ;; need a second shell.
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
