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


; Primitive debugging/logging scheme.  Change *debug* to T to get verbose
; debugging messages.

(defvar *debug* nil)

(defmacro debug (&rest args)
  `(when *debug*
     (format t "Shellpool: ")
     (format t ,@args)
     (force-output)))


; Simple string utilities.

(defconstant nl
  ;; A string with a newline character.
  (coerce (list #\Newline) 'string))

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
  ;; Determines if the string X comes at the start of the string Y.
  ;; For instance, (strprefixp "foo" "foobar") is true.
  (declare (type string x y))
  (strprefixp-impl x y 0 0 (length x) (length y)))

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
  ;; Determines the position of the first occurrence of the substring X in the
  ;; string Y, or returns NIL if there is no such occurrence.
  (declare (type string x y))
  (strpos-fast x y 0 (length x) (length y)))

(defun strsuffixp (x y)
  ;; Determines if the string X comes at the end of the string Y.
  ;; For instance, (strsuffixp "bar" "foobar") is true.
  (declare (type string x y))
  (let* ((xlen (length x))
         (ylen (length y)))
    (and (<= xlen ylen)
         (strprefixp-impl x y 0 (- ylen xlen) xlen ylen))))






; We look for certain strings to know when the program's output ends.  This is
; horribly gross, but in practice it should work.

(defconstant +exit-line+   "HORRIBLE_STRING_TO_DETECT_END_OF_SHELLPOOL_COMMAND")
(defconstant +status-line+ "HORRIBLE_STRING_TO_DETECT_SHELLPOOL_EXIT_STATUS")
(defconstant +pid-line+    "SHELLPOOL_PID")



; Threads:
;
;  - runner-thread runs ordinary (foreground) programs
;  - background-thread runs programs in the background (doesn't wait)
;  - killer-thread is used to kill programs

(defvar *runner-thread* nil)
(defvar *killer-thread* nil)
(defvar *background-thread* nil)


(defun stop ()
  ;; Stops any Shellpool threads that are running.

  (progn (ignore-errors
           (when *runner-thread*
             (debug "STOP: stopping *runner-thread*~%")
             (ccl:signal-external-process *runner-thread* 9)
             (setq *runner-thread* nil)))
         (ignore-errors
           (when *killer-thread*
             (debug "STOP: stopping *killer-thread*~%")
             (ccl:signal-external-process *killer-thread* 9)
             (setq *killer-thread* nil)))
         (ignore-errors
           (when *background-thread*
             (debug "STOP: stopping *background-thread*~%")
             (ccl:signal-external-process *background-thread* 9)
             (setq *background-thread* nil)))
         nil))

(defun start ()
  ;; Stops any Shellpool threads and starts new ones.

  (progn (debug "START: killing old processes~%")
         (stop)
         (debug "START: starting *runner-thread*~%")
         (setf *runner-thread* (ccl:run-program "/bin/bash" nil
                                          :wait nil
                                          :input :stream
                                          :output :stream
                                          :error :stream))
         (debug "START: starting *killer-thread*~%")
         (setf *killer-thread* (ccl:run-program "/bin/bash" nil
                                                 :wait nil
                                                 :input :stream
                                                 :output :stream
                                                 :error :stream))
         (debug "START: starting *background-thread*~%")
         (setf *background-thread* (ccl:run-program "/bin/bash" nil
                                             :wait nil
                                             :input :stream
                                             :output nil
                                             :error nil))
         nil))

(defun check-threads-alive ()
  (and (ccl::external-process-p *runner-thread*)
       (ccl::external-process-p *killer-thread*)
       (ccl::external-process-p *background-thread*)
       (eq (ccl:external-process-status *runner-thread*) :running)
       (eq (ccl:external-process-status *killer-thread*) :running)
       (eq (ccl:external-process-status *background-thread*) :running)))

(defun maybe-start ()
  ;; Stops any tshell processes and starts new ones.
  (unless (check-threads-alive)
    (debug "START: starting *runner-thread*~%")
    (setf *runner-thread* (ccl:run-program "/bin/bash" nil
                                     :wait nil
                                     :input :stream
                                     :output :stream
                                     :error :stream))
    (debug "START: starting *killer-thread*~%")
    (setf *killer-thread* (ccl:run-program "/bin/bash" nil
                                            :wait nil
                                            :input :stream
                                            :output :stream
                                            :error :stream))
    (debug "START: starting *background-thread*~%")
    (setf *background-thread* (ccl:run-program "/bin/bash" nil
                                        :wait nil
                                        :input :stream
                                        :output nil
                                        :error nil)))
  nil)

(defun parse-status-line (line)
  ;; Returns (PREFIX STATUS)
  ;; If it's an exit line, PREFIX is anything that was printed before the
  ;; exit message stuff (which can happen when the command doesn't print a
  ;; newline at the end of its output), and STATUS is an integer that gives
  ;; the exit status code.
  ;; If it's not an exit line, PREFIX and STATUS are both NIL.
  (let ((pos (strpos +status-line+ line)))
    (if (not pos)
        (values nil nil)
      (progn
        (debug "Found status line: ~a~%" line)
        (let ((prefix (subseq line 0 pos))
              (suffix (subseq line (+ 1 (length +status-line+) pos))))
          (multiple-value-bind (val next-pos)
              (parse-integer suffix)
            (declare (ignore next-pos))
            (values prefix val)))))))

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
  (let* ((killer-in (ccl:external-process-input-stream *killer-thread*)))

; Wow, this is tricky.  Want to kill not only the process, but all processes
; that it spawns.  To do this:
;   1. First look up the process's parent, i.e., the bash that is running
;      inside of *runner-thread*.
;   2. Find all processes with *runner-thread* as their parent, removing *runner-thread*
;      itself.
;   3. Kill everything found in 2.

    (format killer-in "PARENT=`ps -o pgrp ~a | tail -1`~%" pid)
    (format killer-in "NOT_PARENT=`pgrep -g $PARENT | grep -v $PARENT`~%")
    (format killer-in "kill -9 $NOT_PARENT~%")
    (force-output killer-in)))

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

(defun run (cmd &key (each-line 'default-each-line))

  (unless (check-threads-alive)
    (error "Invalid *runner-thread*, *killer-thread*, or *background-thread* -- did you call (start)?"))

  (let* ((tshell-in     (ccl:external-process-input-stream *runner-thread*))
         (tshell-out    (ccl:external-process-output-stream *runner-thread*))
         (tshell-err    (ccl:external-process-error-stream *runner-thread*))
         (pid           0)
         (exit-status   1)
         (line          nil)
         (stdout-exit   nil)
         (stderr-exit   nil)
         (temp1
          (cl-fad:with-output-to-temporary-file (stream :template "shellpool-%.tmp")
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
              "(((bash " (namestring temp1)
                  " < /dev/null | sed -u 's/^/+/') 3>&1 1>&2 2>&3 | sed -u 's/^/-/') 2>&1"
                  " ; printf \"\\n" +status-line+ " $?\\\n\" ) &" nl
               "echo " +pid-line+ " $! 1>&2" nl
               "wait" nl
               "echo " +exit-line+ nl
               "echo " +exit-line+ " 1>&2" nl)))

    (debug "Temp path is ~s~%" (namestring temp1))

    (debug "<Bash Commands>~%~a~%</Bash Commands>~%" cmd)

    (write-line cmd tshell-in)
    (finish-output tshell-in)

    (setq pid (parse-pid-line (read-line tshell-err)))

    (debug "PID is ~a.~%" pid)

    (unwind-protect

        (progn
          ;; Read command output until we find the exit line.
          (loop do
                (debug "Reading output line~%")
                (setq line (read-line tshell-out))
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

                      (t
                       (multiple-value-bind
                        (prefix code)
                        (parse-status-line line)
                        (when code
                          (debug "Exit code line, prefix=~s, code=~s~%" prefix code)
                          (setq exit-status code)
                          (unless (equal prefix "")
                            (error "Unexpected prefix ~a~%" prefix)))))))

          ;; Read stderr until we find the exit line.
          (loop do
                (setq line (read-line tshell-err))
                (debug "** Stderr line: ~s~%" line)
                (when (equal line +exit-line+)
                  (debug "TSHELL_EXIT on STDERR: ~a~%" line)
                  (setq stderr-exit t)
                  (loop-finish))
                (debug "TSHELL_ERR: ~a.~%" line)))

      (progn
        ;; Cleanup in case of interrupts.
        (when (not stdout-exit)
          (format t "~%; Note: tshell shutting down process ~a.~%" pid)
          (kill pid)
          (loop do
                (setq line (read-line tshell-out))
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
                (setq line (read-line tshell-err))
                (when (strsuffixp +exit-line+ line)
                  (debug "TSHELL_RECOVER: TSHELL_EXIT on STDERR.~%")
                  (debug "stderr line: ~s, suffixp: ~a~%"
                                line (strsuffixp +exit-line+ line))
                  (loop-finish))
                (debug "TSHELL_RECOVER stderr: Skip ~a on stderr.~%" line)))))

    (delete-file temp1)

    (debug "TSHELL_RUN done.~%")

    (unless (and stdout-exit stderr-exit)
      (error "Somehow didn't exit?"))

    exit-status))


(defun run-background (cmd)
  (unless (check-threads-alive)
    (error "Invalid *runner-thread*, *killer-thread*, or *background-thread* -- did you call (start)?"))

  (let* ((tshell-bg-in (ccl:external-process-input-stream *runner-thread*))
         (cmd (concatenate 'string "(" cmd ") &" nl)))
    (debug "TSHELL_BG~%~a~%" cmd)
    (write-line cmd tshell-bg-in)
    (finish-output tshell-bg-in))

  nil)


