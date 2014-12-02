Shellpool Documentation
=======================

See also the [Installation](INSTALL.md) instructions.

**Basic example**

```
$ ccl                                ;; start lisp
? (ql:quickload :shellpool)          ;; load shellpool (see installation instructions!)
? (shellpool:start)                  ;; start up supporting bash shells
? (shellpool:run "echo hello")       ;; run a command
hello                                ;; some output printed by the command
0                                    ;; the resulting exit status
```


# BOZO document me


# Basic Examples


# Starting and Stopping


# Running Commands

### `(run cmd [options]) --> exit-status`

The **cmd** must be a string and governs what to execute.  This command will be
written into a temporary shell script that will be run with `bash`.  This has
many consequences, for instance:

  - You need to be careful about properly escaping things
  - You can freely use output redirection
  - You can actually give the whole guts of a bash shell script here

The script will be run with `/dev/null` as its input.  This isn't suitable for
running scripts that need to prompt the user for input.

The `run` function waits for the script to finish so that it can give you the
exit status.  For instance, you can expect:

  ```(time (shellpool:run "sleep 5"))```

to report something like 5.01 seconds.

By default, output that **cmd** prints to standard output will be printed to
`*standard-output*`, and output that it prints to standard error will be
printed to `*error-output*`.  So, one way to redirect the output from your
command is to just `let`-binding these streams.  For instance, you could do:

```
? (let* ((stream            (make-string-output-stream))
         (*standard-output* stream))
    (shellpool:run "echo Hello")
    (shellpool:run "echo World")
    (get-output-stream-string stream))
"Hello
World
"```






