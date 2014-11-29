(ql:quickload "shellpool")

(shellpool:start)



(defun run-and-gather (cmd)
  "Runs command, gathering output, returns (values status stdout stderr)."
  (format t "** Running command ~s~%" cmd)
  (let* ((stdout nil)
         (stderr nil)
         (each-line (lambda (line type)
                      (format t "  (~s) ~s~%" type line)
                      (force-output)
                      (case type
                        (:stdout (push line stdout))
                        (:stderr (push line stderr))
                        (otherwise (error "Bad type ~s for line ~s~%" type line)))))
         (status (shellpool:run cmd :each-line each-line))
         (stdout (nreverse stdout))
         (stderr (nreverse stderr)))
    (values status stdout stderr)))

(defun check (name expected actual)
  (if (equal expected actual)
      (format t "OK ~a~%" name)
    (format t "FAIL ~a: Expected ~s, Found ~s~%" name expected actual)))

(defun basic-test (cmd &key
                       (status '0)
                       (stdout 'nil)
                       (stderr 'nil))
  (multiple-value-bind
   (actual-status actual-stdout actual-stderr)
   (run-and-gather cmd)
   (check "status" status actual-status)
   (check "stdout" stdout actual-stdout)
   (check "stderr" stderr actual-stderr)))


; Trivial tests:

(basic-test "echo hello"
            :stdout '("hello"))

(basic-test "echo hello 1>&2 "
            :stderr '("hello"))

(basic-test "exit 1"
            :status 1)

(basic-test "exit 15"
            :status 15)


; This read test is meant to ensure that the subcommand really doesn't get any
; input stream attached to it.  Perhaps we would one day want to change how
; this works to allow hooking up streams to commands, but that seems pretty
; hard and scary.  What if they send Ctrl+D and end the stream or something?
; Could we lose our bash shell?

(basic-test "read -p 'Will this work?' yn"
            :status 1)


; Test some more complex output interleaving.

(basic-test "./test1.sh"
            :status 0
            :stdout '("stdout line 1" "stdout line 2" "stdout line 3")
            :stderr '("stderr line 1" "stderr line 2"))


; Like the previous test, but the output should be produced (and streamed)
; incrementally.  You should notice lag as the messages are printed, but you
; should see each message as it becomes available.

(basic-test "./test2.sh"
            :status 2
            :stdout '("stdout line 1" "stdout line 2" "stdout line 3")
            :stderr '("stderr line 1" "stderr line 2"))


; Now some tests of garbage commands.

(defun check-bad-input (cmd &key
                            (status '2)
                            (stdout 'nil))
  (multiple-value-bind
   (actual-status actual-stdout actual-stderr)
   (run-and-gather cmd)
   (check "status" status actual-status)
   (check "stdout" stdout actual-stdout)
   (or (consp actual-stderr)
       (error "Expected an error message."))))

(check-bad-input "echo \"oops, forgot ending quote")

(basic-test "echo hello"
            :stdout '("hello"))

(check-bad-input "echo 'oops, forgot ending single quote")

(basic-test "echo hello"
            :stdout '("hello"))

(check-bad-input "(echo \"Oops, no closing paren.\" ")

(basic-test "echo hello"
            :stdout '("hello"))



(setq shellpool:*debug* t)

;;  - handling of backgrounded commands, etc.
;;  - argument parsing stuff, execve style stuff
 




;; (let* ((proc (ccl:run-program "./harness2.sh" nil
;;                               :wait nil
;;                               :input :stream
;;                               :output :stream
;;                               :error :stream))
;;        (stdout (ccl::external-process-output-stream proc))
;;        (stderr (ccl::external-process-output-stream proc)))
;;   (declare (ignorable stdout stderr))

;;   ;; (let ((foo (read-line stderr nil)))
;;   ;;   (format t "Got stderr line~s~%" foo)
;;   ;;   (force-output))

;;   (loop do
;;         (let ((line (read-line stdout nil)))
;;           (format t "Got line ~s~%" line)
;;           (force-output)
;;           (when (not line)
;;             (loop-finish)))))

