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

(let ((lock      (bt:make-lock))  ;; Protects kill-now and kill-done
      (kill-now  nil)             ;; Turns on when it's time to kill the subprocess
      (kill-done nil)             ;; Set by unwind-protect whenever the command exits
      (runner    nil)             ;; The thread that will run the test
      )

  (defun interrupt-runner ()
    (msg " - Sending interrupt to runner thread.~%")
    (bt:interrupt-thread runner
                         (lambda ()
                           (msg " - In runner thread, now doing interrupt.~%")
                           (throw 'shellpool-test-tag "Interrupt from do-test"))))

  (defun start-runner (cmd ready-fn)
    (msg " - Starting runner thread.~%")
    (let* ((each-line (lambda (line type)
                        (declare (ignore type))
                        (msg " - OUT: ~s~%" line)
                        (when (funcall ready-fn line)
                          (msg " - Setting READY for kill.~%")
                          (bt:with-lock-held (lock)
                                             (setq kill-now t)))))
           (result (catch 'shellpool-test-tag
                     (unwind-protect
                         (shellpool:run cmd :each-line each-line)
                       (progn
                         (msg " - Setting kill-done to true.~%")
                         (bt:with-lock-held (lock)
                                            (setq kill-done t)))))))
    (msg " - Killtest thread got result: ~s~%" result)))

  (defun wait-until-kill-now ()
    (msg " - Waiting for the start signal.~%")
    (loop do
          (bt:with-lock-held (lock)
                             (when kill-now
                               (msg " - Got kill-now signal.~%")
                               (return-from wait-until-kill-now nil)))))

  (defun wait-until-kill-done ()
    (msg " - Waiting for kill-done signal.~%")
    (loop do
          (bt:with-lock-held (lock)
                             (when kill-done
                               (msg " - Got kill-done signal.~%")
                               (return-from wait-until-kill-done nil)))))

  (defun do-test (&key cmd      ; Command to run
                       subname  ; Name of the subprocess that must be killed
                       ready-fn ; Function to determine if it's time to kill
                       max-time ; Time bound in seconds
                       )
    (when (has-process subname)
      (error "Looks like ~s is running already, won't be able to test killing correctly."
             subname))
    (let ((start-time (get-internal-run-time)))
      ;; In the runner thread, start sleeping for 10 seconds.
      (setq runner (bt:make-thread
                    (lambda () (start-runner cmd ready-fn))))
      ;; Wait for the first line of output from the command (which it prints
      ;; right away when it starts running.)
      (wait-until-kill-now)
      ;; Now that we've got the first line of output, we're sure the command
      ;; should be running.  So let's try to verify that it is indeed running.
      ;; BOZO it doesn't seem like we should have to sleep here, but sometimes
      ;; even after getting the first line of output, the process list doesn't
      ;; seem to include sleep.pl yet, so sleep a moment before checking
      (sleep .2)
      (unless (has-process subname)
        (error "Doesn't seem like ~s got started." subname))
      ;; Since it is running, let's now try to interrupt it.
      (interrupt-runner)
      ;; Wait till the KILL-DONE flag gets set.  Hopefully it is set because
      ;; the command was killed by our interrupt.
      (wait-until-kill-done)
      ;; Try to verify that the process indeed got killed as desired.
      ;; It might take a moment for the OS to kill the process, so sleep
      ;; for a short while first.  BOZO it would be nice to not need to
      ;; do this sleeping.
      (sleep .2)
      (when (has-process subname)
        (error "Doesn't seem like ~s got killed." subname))
      ;; Try to verify that all of the above happened very fast, i.e., we
      ;; didn't just sit around waiting for the command to exit.
      (let* ((end-time (get-internal-run-time))
             (elapsed  (coerce (- end-time start-time) 'float))
             (limit    (* max-time internal-time-units-per-second)))
        (msg " - Test passed in ~s seconds.~%~%" (/ elapsed internal-time-units-per-second))
        (sleep .2)
        (unless (< elapsed limit)
          (error "Seems like that took too long."))))))

(shellpool:start) ;; need a second shell.

; (setq shellpool:*debug* t)

;; Basic check: can we kill the main process that gets launched?  We run this a
;; few times to try to make sure our killing stuff works more than once.
(loop for i from 1 to 5 do
      (msg "*** Starting basic sleep test ~s.~%" i)
      (do-test :cmd "./sleep.pl 10"
               :subname "sleep.pl"
               :ready-fn (lambda (line)
                           (msg " --- Got line: ~s~%" line)
                           t)
               :max-time 3))

;; Check of whether we can kill subprocesses that our command launches.
(loop for i from 1 to 5 do
      (format t "*** Starting sleepN test ~s.~%" i)
      (do-test :cmd "./sleepN.sh 10 5"
               :subname "sleep.pl"
               :ready-fn (lambda (line)
                           (msg " --- Got line: ~s~%" line)
                           (if (equal line "Waiting for sleep.pl processes to finish.")
                               (progn
                                (msg " --- Ready to kill.~%")
                                t)
                             nil))
               :max-time 3))

;; Check whether we can kill off a "bad" process that ignores various kill signals.
(loop for i from 1 to 5 do
      (msg "*** Starting badsleep test ~s.~%" i)
      (do-test :cmd "./badsleep.pl 10"
               :subname "badsleep.pl"
               :ready-fn (lambda (line)
                           (msg " --- Got line: ~s~%" line)
                           t)
               :max-time 3))

;; And similarly for a process that launches "bad" processes.
(loop for i from 1 to 5 do
      (format t "*** Starting badsleepN test ~s.~%" i)
      (do-test :cmd "./badsleepN.sh 10 5"
               :subname "badsleep.pl"
               :ready-fn (lambda (line)
                           (msg " --- Got line: ~s~%" line)
                           (if (equal line "Waiting for badsleep.pl processes to finish.")
                               (progn
                                (msg " --- Ready to kill.~%")
                                t)
                             nil))
               :max-time 3))


;; OK, so the pgrp based kill seems to work well, but perhaps a simpler
;; method would work?
