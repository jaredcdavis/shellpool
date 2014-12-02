; Shellpool - Interface from Common Lisp to External Programs
; Copyright (C) 2014 Kookamara LLC
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

(in-package :shellpool)


; ----------------------------------------------------------------------------
; Simple Utilities

(defvar *debug* nil
  "Change to T for verbose debugging messages.")

(defmacro debug (&rest args)
  "Like (format t ...) but only prints if debugging is enabled."
  `(when *debug*
     (format t "Shellpool: ")
     (format t ,@args)
     (force-output)))

(defconstant nl
  (coerce (list #\Newline) 'string)
  "Newline as a string.  Convenient in string concatenations as a way to insert
   newlines without having to ruin your indenting.")

(defun strprefixp-impl (x y xn yn xlen ylen)
  (declare (type string x y)
           (type fixnum xn yn xlen ylen))
  (cond ((eql xn xlen)
         t)
        ((eql yn ylen)
         nil)
        ((eql (char x xn) (char y yn))
         (strprefixp-impl x y
                          (the fixnum (+ xn 1))
                          (the fixnum (+ yn 1))
                          xlen ylen))
        (t
         nil)))

(defun strprefixp (x y)
  "Determines if the string X comes at the start of the string Y.
   Example:  (strprefixp \"foo\" \"foobar\") is true."
  (declare (type string x y))
  (strprefixp-impl x y 0 0 (length x) (length y)))

(defun strsuffixp (x y)
  "Determines if the string X comes at the end of the string Y.
   Example: (strsuffixp \"bar\" \"foobar\") is true."
  (declare (type string x y))
  (let* ((xlen (length x))
         (ylen (length y)))
    (and (<= xlen ylen)
         (strprefixp-impl x y 0 (- ylen xlen) xlen ylen))))

(defun strpos-fast (x y n xlen ylen)
  (declare (type string x y)
           (type fixnum n xlen ylen))
  (cond ((strprefixp-impl x y 0 n xlen ylen)
         n)
        ((eql n ylen)
         nil)
        (t
         (strpos-fast x y (the fixnum (+ 1 n)) xlen ylen))))

(defun strpos (x y)
  "Determines the position of the first occurrence of the substring X in the
   string Y, or returns NIL if there is no such occurrence."
  (declare (type string x y))
  (strpos-fast x y 0 (length x) (length y)))




; ----------------------------------------------------------------------------
; Shells

; BOZO we probably don't really need separate KILLER and BACKGROUND shells.  We
; could probably fuse them together into a dedicated AUX shell pretty easily.

(defstruct state

  ;; LOCK is a lock that protects access to the KILLER and BACKGROUND shells.
  ;; It must also be acquired whenever modifying RUNNERS.
  (lock)

  ;; KILLER is a dedicated bash process that is used to send kill signals to
  ;; subsidiary processes to support interrupts.  Note that we only need a
  ;; single KILLER shell to be able to kill any number of running processes.
  ;; Locking convention: the LOCK must be held while using the KILLER thread.
  ;; We expect uses of KILLER to take very little time (it just sends a "kill"
  ;; signal to some other process), so a single process should suffice.
  (killer)

  ;; BACKGROUND is a dedicated bash process that is used to launch programs in
  ;; the background when RUN-BACKGROUND is called.  Since background processes
  ;; are of a "fire and forget" nature, i.e., we invoke commands like "firefox
  ;; &", we think a single background shell should suffice.  Locking
  ;; convention: the LOCK must be held while using the BACKGROUND thread.
  (background)

  ;; SEM is a semaphore that governs access to the RUNNERS.  Invariant: the
  ;; count of the semaphore should always agree exactly with (LENGTH RUNNERS).
  ;; See GET-RUNNER, which properly waits for the next free runner.
  (sem)

  ;; RUNNERS are an ordinary list of bash processes that are available for
  ;; running foreground shell commands with RUN.
  (runners))

(defparameter *state*
  (make-state :lock       (bt:make-lock)
              :killer     nil
              :background nil
              :sem        (bt-sem:make-semaphore)
              :runners    nil))

(defmacro with-runner (name &rest forms)
  `(let ((,name nil))
     (unwind-protect
         (progn
           (unless (bt-sem:wait-on-semaphore (state-sem *state*))
             (error "Failed to acquire runner"))
           (bt:with-lock-held ((state-lock *state*))
                              (unless (consp (state-runners *state*))
                                ;; Should never happen.  The number of free runners
                                ;; should always agree with the value of the
                                ;; semaphore.
                                (error "Our turn to go, but no runners are available?"))
                              (setq ,name (pop (state-runners *state*))))
           ;; Got the runner, so use it to run stuff.
           ,@forms)
       ;; Clean up by putting the runner back.
       (bt:with-lock-held ((state-lock *state*))
                          (push ,name (state-runners *state*))
                          (bt-sem:signal-semaphore (state-sem *state*))))))

(defun make-new-runner ()
  (ccl:run-program "/bin/bash" nil
                   :wait   nil
                   :input  :stream
                   :output :stream
                   :error  :stream))

(defun add-runners (n)
  (bt:with-lock-held ((state-lock *state*))
                     (loop for i from 1 to n do
                           (push (make-new-runner) (state-runners *state*)))
                     (bt-sem:signal-semaphore (state-sem *state*) n)))


(defun shell-alive-p (x)
  (and (ccl::external-process-p x)
       (eq (ccl:external-process-status x) :running)))


;; (defun start (&key (runners '1))
;;   (let ((state *state*))
;;     (when (state-lock state)
;;       (
      
      
;;       (add-runners runners)
;;     (setq *state*
;;           (start-
  
  



; Threads:
;
;  - runner-thread runs ordinary (foreground) programs
;  - background-thread runs programs in the background (doesn't wait)
;  - killer-thread is used to kill programs

(defvar *runner-shell* nil)
;(defvar *killer-shell* nil)
;(defvar *background-shell* nil)


(defun stop ()
  ;; Stops any Shellpool threads that are running.

  (progn (ignore-errors
           (when *runner-shell*
             (debug "STOP: stopping *runner-shell*~%")
             (ccl:signal-external-process *runner-shell* 9)
             (setq *runner-shell* nil)))
         ;; BOZO need lock
         (ignore-errors
           (when (state-killer *state*)
             (debug "STOP: stopping killer shell~%")
             (ccl:signal-external-process (state-killer *state*) 9)
             (setq (state-killer *state*) nil)))
         ;; BOZO need lock
         (ignore-errors
           (when (state-background *state*)
             (debug "STOP: stopping background shell~%")
             (ccl:signal-external-process background-shell 9)
             (setq background-shell nil)))
         nil))

(defun start ()
  ;; Stops any Shellpool threads and starts new ones.

  (progn (debug "START: killing old processes~%")
         (stop)
         (debug "START: starting *runner-shell*~%")
         (setf *runner-shell* (ccl:run-program "/bin/bash" nil
                                          :wait nil
                                          :input :stream
                                          :output :stream
                                          :error :stream))
         (debug "START: starting killer shell~%")
         ;; bozo need lock
         (setf (state-killer *state*)
               (ccl:run-program "/bin/bash" nil
                                :wait nil
                                :input  :stream
                                :output :stream
                                :error  :stream))
         (debug "START: starting *background-shell*~%")
         ;; bozo need lock
         (setf (state-background state)
               (ccl:run-program "/bin/bash" nil
                                :wait nil
                                :input :stream
                                :output nil
                                :error nil))
         (add-runners 1)
         nil))

(defun check-threads-alive ()
  (and (shell-alive-p *runner-shell*)
       ;; BOZO need lock
       (shell-alive-p (state-background *state*))
       (shell-alive-p (state-killer *state*))))

;; (defun maybe-start ()
;;   ;; Stops any tshell processes and starts new ones.
;;   (unless (check-threads-alive)
;;     (debug "START: starting *runner-shell*~%")
;;     (setf *runner-shell* (ccl:run-program "/bin/bash" nil
;;                                      :wait nil
;;                                      :input :stream
;;                                      :output :stream
;;                                      :error :stream))
;;     (debug "START: starting *killer-shell*~%")
;;     (setf *killer-shell* (ccl:run-program "/bin/bash" nil
;;                                             :wait nil
;;                                             :input :stream
;;                                             :output :stream
;;                                             :error :stream))
;;     (debug "START: starting *background-shell*~%")
;;     (setf *background-shell* (ccl:run-program "/bin/bash" nil
;;                                         :wait nil
;;                                         :input :stream
;;                                         :output nil
;;                                         :error nil)))
;;   nil)

(defconstant +exit-line+   "SHELLPOOL_EXIT")
(defconstant +status-line+ "SHELLPOOL_STATUS")
(defconstant +pid-line+    "SHELLPOOL_PID")

(defun parse-pid-line (line)
  ;; Given a line like TSHELL_PID 1234, we return 1234.
  (debug "Parsing PID line: ~a~%" line)
  (unless (strprefixp +pid-line+ line)
    (error "TSHELL error: bad pid line: ~a." line))
  (multiple-value-bind (val pos)
      (parse-integer (subseq line (+ 1 (length +pid-line+))))
    (declare (ignore pos))
    val))

(defun kill (pid)
  ;; Use the killer thread to try to kill process PID.
  (debug "KILL: killing ~a.~%" pid)
  (bt:with-lock-held
   ((state-lock *state*))
   (let ((killer    (state-killer *state*))
         (killer-in (ccl:external-process-input-stream killer)))

; Wow, this is tricky.  Want to kill not only the process, but all processes
; that it spawns.  To do this:
;   1. First look up the process's parent, i.e., the bash that is running
;      inside of the runner
;   2. Find all processes with runner as their parent, removing runner itself
;   3. Kill everything found in 2.

; BOZO this may all be different now that we have a subshell running our stuff.

    (format killer-in "PARENT=`ps -o pgrp ~a | tail -1`~%" pid)
    (format killer-in "NOT_PARENT=`pgrep -g $PARENT | grep -v $PARENT`~%")
    (format killer-in "kill -9 $NOT_PARENT~%")
    (force-output killer-in))))

(defun default-each-line (line type)
  (declare (type string line))
  (debug "Default streaming line (type ~s): ~s ~%" type line)
  (case type
    (:stdout
     (write-line line)
     (force-output))
    (:stderr
     (write-line line *error-output*)
     (force-output *error-output*))
    (otherwise
     (error "Unexpected line type ~s" type))))


(defun run (cmd &key (each-line #'default-each-line))
  (check-type cmd string)
  (check-type each-line function)

  (unless (check-threads-alive)
    (error "Invalid *runner-shell*, killer shell, or background shell -- did you call (start)?"))

  (with-runner runner

    (let* ((bash-in     (ccl:external-process-input-stream runner))
           (bash-out    (ccl:external-process-output-stream runner))
           (bash-err    (ccl:external-process-error-stream runner))
           (pid           nil)
           (exit-status   nil)
           (line          nil)
           (stdout-exit   nil)
           (stderr-exit   nil)
           (tempfile      (cl-fad:with-output-to-temporary-file
                           (stream :template "shellpool-%.tmp")
                           (write-line "#!/bin/sh" stream)
                           (write-line cmd stream)))

; Extremely tricky and carefully crafted bash code follows.
;
; The core of this is basically the following:
;
;  0.     set -o pipefail
;  1.     ((bash cmd.sh < /dev/null | sed -u 's/^/+/') 3>&1 1>&2 2>&3 | sed -u 's/^/-/') 2>&1
;
; What's going on here?
;
;   - The SED commands and output redirection are grabbing the output from cmd.sh
;     and modifying it so that:
;          every stdout line prefixed by +
;          every stderr line prefixed by -
;          the resulting lines are merged together and printed to stdout
;          the use of "-u" prevents sed from adding extra buffering
;
;     This is wonderful and allows us to
;       (1) distinguish the stderr/stdout lines from one another (obviously),
;       (2) distinguish the command output from other stuff (needed in a moment),
;       (3) get stdout/stderr together, interleaved, as they are produced.
;
;   - Normally this use of sed would ruin the exit code from cmd.sh.  However,
;     the pipefail option corrects for this, and sets things up so that if
;     cmd.sh exits with a failure, we'll get this failure as the exit status
;     for the whole pipeline.
;
; We extend this core with some additional stuff for being able to extract the
; exit code and PID of the command.  Here is the real solution:
;
;  0.   set -o pipefail                        # As above, and doesn't bother anything below
;  1.   ( <core 1> ; printf "\nEXIT $?\n") &   # Run in background (so we can get PID), print exit status
;  2.   echo PID $! 1>&2                       # Print the PID to STDOUT.
;  3.   wait                                   # Wait for the command to finish
;  4.   echo ""                                # Make sure we get a newline at end of stdout
;  5.   echo "" 1>&2                           # Make sure we get a newline at end of stderr
;  6.   echo END                               # Print end-of-command to stdout
;  7.   echo END 1>&2                          # Print end-of-command to stderr
;
; The main trick here is to run the command in the background and then to wait
; for it.  We do this so that (line 2) we can print out the PID associated with
; the subshell we're launching.  Notice that something subtle helps to make
; this safe: all output from Line 1 goes to stdout, but we print the PID to
; stderr.  Accordingly, when reading output in Lisp, we can be sure that the
; first line of stderr is going to be the PID line, even if the command prints
; immediately to stderr.
;
; The printf command in line 1 deserves some attention.  Note that we add a
; newline before printing the exit message.  This is because the command could
; exit after printing some non newline-terminated text, e.g., suppose the user
; wants to run a command like "echo -n hello".  By printing the newline, we're
; sure that our EXIT message will occur on its own line.  This makes it
; possible to reliably parse it without any restrictions on what core might
; print.
;
; The end strings are needed to determine when we've reached the end of the
; output associated with this command.

           (cmd (concatenate 'string
                             "set -o pipefail" nl
                             "(((bash " (namestring tempfile)
                             " < /dev/null | sed -u 's/^/+/') 3>&1 1>&2 2>&3 | sed -u 's/^/-/') 2>&1"
                             " ; printf \"\\n" +status-line+ " $?\\\n\" ) &" nl
                             "echo " +pid-line+ " $! 1>&2" nl
                             "wait" nl
                             "echo " +exit-line+ nl
                             "echo " +exit-line+ " 1>&2" nl)))

      (debug "Temp path is ~s~%" (namestring tempfile))

      (debug "<Bash Commands>~%~a~%</Bash Commands>~%" cmd)

      (write-line cmd bash-in)
      (finish-output bash-in)

      (setq pid (parse-pid-line (read-line bash-err)))

      (debug "PID is ~a.~%" pid)

      (unwind-protect

          (progn
            ;; Read command output until we find the exit line.
            (loop do
                  (setq line (read-line bash-out))
                  (debug "** Output line: ~s~%" line)
                  (cond ((equal line "")
                         (debug "Ignoring blank line.~%"))
                        ((equal line +exit-line+)
                         (debug "Exit line, done reading STDOUT.~%")
                         (setq stdout-exit t)
                         (loop-finish))
                        ((eql (char line 0) #\+)
                         (debug "Stdout line, invoking callback.~%")
                         (funcall each-line (subseq line 1 nil) :stdout))
                        ((eql (char line 0) #\-)
                         (debug "Stderr line, invoking callback.~%")
                         (funcall each-line (subseq line 1 nil) :stderr))
                        ((strprefixp +status-line+ line)
                         (debug "Exit status line: ~s~%" line)
                         (setq exit-status
                               (parse-integer line :start (+ 1 (length +status-line+)))))
                        (t
                         (error "Unexpected line ~s~%" line))))

            ;; Read stderr until we find the exit line.
            (loop do
                  (setq line (read-line bash-err))
                  (debug "** Stderr line: ~s~%" line)
                  (cond ((equal line "")
                         (debug "Ignoring blank line.~%"))
                        ((equal line +exit-line+)
                         (debug "Exit line, done reading STDERR.~%")
                         (setq stderr-exit t)
                         (loop-finish))
                        (t
                         (error "Unexpected line ~s~%" line)))))

        (progn
          ;; Cleanup in case of interrupts.
          (when (not stdout-exit)
            (format t "~%; Note: tshell shutting down process ~a.~%" pid)
            (kill pid)
            (loop do
                  (setq line (read-line bash-out))
                  (when (strsuffixp +exit-line+ line)
                    ;; We used to try to match +exit-line+ exactly, but
                    ;; then we found that if we interrupt while the program has
                    ;; printed partial output, we can end up with a situation
                    ;; like:
                    ;;     <partial output>HORRIBLE_STRING_TO_DETECT_WHATEVER
                    ;; So now we are more permissive.  We don't try to capture
                    ;; the <partial output> because we're just skipping these
                    ;; lines anyway.
                    (debug "TSHELL_RECOVER: TSHELL_EXIT on STDOUT.~%")
                    (debug "stdout line: ~s, suffixp: ~a~%"
                           line (strsuffixp +exit-line+ line))
                    (loop-finish))
                  (debug "TSHELL_RECOVER stdout: Skip ~a.~%" line)))

          (when (not stderr-exit)
            (loop do
                  (setq line (read-line bash-err))
                  (when (strsuffixp +exit-line+ line)
                    (debug "TSHELL_RECOVER: TSHELL_EXIT on STDERR.~%")
                    (debug "stderr line: ~s, suffixp: ~a~%"
                           line (strsuffixp +exit-line+ line))
                    (loop-finish))
                  (debug "TSHELL_RECOVER stderr: Skip ~a on stderr.~%" line)))))

      (delete-file tempfile)

      (debug "TSHELL_RUN done.~%")

      (unless (integerp exit-status)
        (error "Somehow didn't get the exit status?"))

      (unless (and stdout-exit stderr-exit)
        (error "Somehow didn't exit?"))

      exit-status)))


(defun run-background (cmd)
  (unless (check-threads-alive)
    (error "Invalid *runner-shell*, killer shell, or background shell -- did you call (start)?"))

  (let* ((tshell-bg-in (ccl:external-process-input-stream *runner-shell*))
         (cmd (concatenate 'string "(" cmd ") &" nl)))
    (debug "TSHELL_BG~%~a~%" cmd)
    (write-line cmd tshell-bg-in)
    (finish-output tshell-bg-in))

  nil)


