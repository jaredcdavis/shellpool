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

(setq shellpool:*debug* t)

(let ((lock (bt:make-lock))  ;; Protects st
      (st   :start))         ;; Current state of our test, see below.

  ;; This is a test harness to check that we can run and kill subprograms as we
  ;; expect.
  ;;
  ;; I originally wrote this test using BT:INTERRUPT-THREAD to make the runner
  ;; thread throw a tag.  That worked fine in CCL and SBCL, but in Allegro it
  ;; seems that you are not allowed to cause a non-local exit from a thread
  ;; when you interrupt it, so this did not work at all!  Accordingly, I
  ;; rewrote the test to be more "cooperative", so that the RUNNER thread will
  ;; throw its own tag when the CHECKER thread is ready.
  ;;
  ;; The flow of the test is as follows.  There are two threads: a RUNNER
  ;; thread and a CHECKER thread.  The CHECKER thread is the main thread so we
  ;; don't explicitly invoke it.  Our current place in the test is recorded in
  ;; ST.  The steps of the test are:
  ;;
  ;;  0. Launch the RUNNER thread.
  ;;
  ;;  1. ST == :START   (Runner's Turn)
  ;;
  ;;       RUNNER:  Invoke the subprogram.
  ;;                Scan the subprogram's output until we see the "ready" line.
  ;;                Change ST to :RUNNING.
  ;;
  ;;       CHECKER: Do nothing, wait until ST becomes :RUNNING.
  ;;
  ;;  2. ST == :RUNNING   (Checker's Turn)
  ;;
  ;;       CHECKER: Ensure the subprogram is running.
  ;;                Change ST to :THROW
  ;;
  ;;       RUNNER:  Do nothing, wait for ST to become :THROW.
  ;;
  ;;  3. ST == :THROW  (Runner's Turn)
  ;;
  ;;       RUNNER:  Throw to a tag to simulate an interrupt.
  ;;                Set ST to :THROWN.
  ;;
  ;;       CHECKER: Do nothing, wait for ST to become :THROWN.
  ;;
  ;;  4. ST == :THROWN  (Checker's Turn)
  ;;
  ;;       CHECKER: Ensure the subprogram is killed.
  ;;                Ensure that we haven't exceeded the permitted time.

  (defun wait-until (desired-st)
    (loop do
          (bt:with-lock-held (lock)
                             (when (eq st desired-st)
                               (return-from wait-until nil)))
          (sleep .2)))

  (defun start-runner (cmd ready-fn)
    (msg " - RUNNER: Hello.~%")
    (let* ((each-line
            (lambda (line type)
              (declare (ignore type))
              (msg " - RUNNER: Got Output: ~s~%" line)
              (bt:with-lock-held
               (lock)
               (cond ((eq st :throw)
                      (msg " - RUNNER: Throwing.~%")
                      (throw 'shellpool-test-tag "My Interrupt"))
                     ((eq st :start)
                      (if (funcall ready-fn line)
                          (progn (msg " - RUNNER: Got READY, Setting ST to :RUNNING.~%")
                                 (setq st :running))
                        (msg " - RUNNER: waiting until ready.~%")))
                     (t
                      (msg " - RUNNER: Ignoring because ST is ~s~%" st))))))
           (result (catch 'shellpool-test-tag
                     (unwind-protect
                         (shellpool:run cmd :each-line each-line)
                       (progn
                         (bt:with-lock-held (lock)
                                            (msg " - RUNNER: Setting ST to :THROWN.~%")
                                            (setq st :thrown)))))))
      (msg " - RUNNER: Goodbye.  Result: ~s~%" result)))

  (defun do-test (&key cmd      ; Command to run
                       subname  ; Name of the subprocess that must be killed
                       ready-fn ; Function to determine if it's time to kill
                       max-time ; Time bound in seconds
                       )
    (bt:with-lock-held (lock)
                       (msg " - CHECKER: Starting test.~%")
                       (setq st :start))
    (when (has-process subname)
      (error "Looks like ~s is running already, won't be able to test killing correctly."
             subname))
    (let ((start-time (get-internal-real-time)))
      (msg " - CHECKER: starting runner thread.~%")
      (bt:make-thread (lambda () (start-runner cmd ready-fn)))
      (msg " - CHECKER: waiting for :RUNNING~%")
      (wait-until :running)
      ;; Now that we've got the first line of output, we're sure the command
      ;; should be running.  So let's try to verify that it is indeed running.
      ;; BOZO it doesn't seem like we should have to sleep here, but sometimes
      ;; even after getting the first line of output, the process list doesn't
      ;; seem to include sleep.pl yet, so sleep a moment before checking
      (sleep .2)
      (msg " - CHECKER: Checking if program is running.~%")
      (unless (has-process subname)
        (error "Doesn't seem like ~s got started." subname))
      ;; Since it is running, let's now try to interrupt it.
      (bt:with-lock-held (lock)
                         (msg " - CHECKER: Program alive, setting ST to ~s~%" st)
                         (setq st :throw))
      (msg " - CHECKER: Waiting until thrown.~%")
      (wait-until :thrown)
      ;; Try to verify that the process indeed got killed as desired.
      ;; It might take a moment for the OS to kill the process, so sleep
      ;; for a short while first.  BOZO it would be nice to not need to
      ;; do this sleeping.
      (sleep .2)
      (msg " - CHECKER: Got thrown.~%")
      (when (has-process subname)
        (error "Doesn't seem like ~s got killed." subname))
      (msg " - CHECKER: Program seems sufficiently dead.~%")
      ;; Try to verify that all of the above happened very fast, i.e., we
      ;; didn't just sit around waiting for the command to exit.
      (let* ((end-time (get-internal-real-time))
             (elapsed  (coerce (- end-time start-time) 'float))
             (limit    (* max-time internal-time-units-per-second)))
        (msg " - Test passed in ~s seconds.~%" (/ elapsed internal-time-units-per-second))
        (sleep .2)
        (unless (< elapsed limit)
          (error "Seems like that took too long.")))
      (sleep .1)
      (msg "~%"))))

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
      (msg "*** Starting sleepN test ~s.~%" i)
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
      (msg "*** Starting badsleepN test ~s.~%" i)
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
