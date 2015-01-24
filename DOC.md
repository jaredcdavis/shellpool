Shellpool Documentation
=======================

See also the [Installation](INSTALL.md) instructions.


## Minimal Example

The typical way to use Shellpool is to `start` some bash processes and
then `run` some commands.

```
$ ccl                                ;; start lisp
? (ql:quickload :shellpool)          ;; load shellpool (see installation instructions!)
? (shellpool:start)                  ;; start a bash shell
? (shellpool:run "echo hello")       ;; run a command
hello                                ;; some output printed by the command
0                                    ;; the resulting exit status
```

Why are `start` and `run` separated?  Launching external programs involves
[forking](http://en.wikipedia.org/wiki/Fork_%28operating_system%29) your Lisp.
Forking is **not reliable** when your Lisp has many GB of memory allocated or
multiple threads are already running.  (Here are
[some](http://www.linuxprogrammingblog.com/threads-and-fork-think-twice-before-using-them)
[articles](http://bryanmarty.com/2012/01/14/forking-jvm/) for background.)
Separating `start` from `run` largely solves these problems: you can `start`
your shells early while your program is booting up, before creating any helper
threads or allocating lots of memory.  You can then freely `run` external
programs from these shells without having to fork Lisp.


## Starting Shells

### `(start [n]) --> nil`

Examples:
```
(shellpool:start)    ;; start a single bash shell
(shellpool:start 3)  ;; start 3 more bash shells
```

The `start` command launches bash shells that can be used for running
sub-commands.  Typically `start` is only called once at the start of your
program, but you can also call it subsequently to start up additional shells,
as shown above.

How many shells should you start?  The number of shells you create will limit
how many simultaneous external programs you can `run` at a time.  So:

 - If your Lisp program is single-threaded, a single shell will be fine.

 - If your Lisp program is multi-threaded (e.g., a web server or similar), and
   you need to be able to invoke external programs from many threads, then you
   may want many shells.  (The `run` command will wait until a shell becomes
   available, so running out of shells might throttle your program.)


## Running Commands

### `(run script [options]) --> exit-status`

Examples:
```
(shellpool:run "echo hello")
(shellpool:run "convert-image photo.png" :each-line #'parse-convert-image)
```

The `run` function runs a command and waits for it to finish (so it can give
you the exit status).  For instance, you can expect `(time (shellpool:run
"sleep 5"))` to report something like 5.001 seconds and return 0.

The `script` to execute must be a string.  This string will be written into a
temporary script that will be run by `bash`.  You can write whole bash scripts
with functions, sequences of commands, etc.

The temporary script will be run with `/dev/null` as its input.  This isn't
suitable for running scripts that need to prompt the user for input.


### Handling Output

By default, output that your `script` prints is forwarded sensibly:

  - output to `stdout` goes to `*standard-output*`
  - output to `stderr` goes to `*error-output*`

An easy way to change where the output goes is to `let`-bind these streams.
For instance, to collect `stdout` instead of streaming it, you could do this:

```
? (let* ((stream            (make-string-output-stream))
         (*standard-output* stream))
    (shellpool:run "echo Hello")
    (shellpool:run "echo World")
    (get-output-stream-string stream))
"Hello
World
"
```

For more flexibility, you can write your own line-handling function.  Your
function will get the `line` to process and its `type` (i.e., whether it is a
`stdout` or `stderr` line).  For example:

```
? (defun my-handle-line (line type)
    (format t "On ~s, got ~s~%" type line))

? (shellpool:run "echo hello
                  echo world 1>&2
                  echo foo
                  echo bar 1>&2
                  exit 7"
                 :each-line #'my-handle-line)
On :STDERR, got "world"
On :STDERR, got "bar"
On :STDOUT, got "hello"
On :STDOUT, got "foo"
7                          ;; <-- return value from (shellpool:run ...)
```

You can use this to collect up output, transform it on the fly, filter it,
stream it to any number of places, etc.  For example, here is a way to
separately collect the stdout and stderr lines without any streaming:

```
? (defun fancy-runner (cmd)
    (let ((out-lines nil)
	  (err-lines nil)
	  (return-code nil))
      (setq return-code
	    (shellpool:run cmd
                           :each-line
                           (lambda (line type)
                             (case type
                               (:STDOUT (push line out-lines))
                               (:STDERR (push line err-lines))
                               (otherwise (error "unexpected line type from shellpool: ~s" type))))))
      (list :return-code return-code
            :out-lines (nreverse out-lines)
            :err-lines (nreverse err-lines))))

? (fancy-runner "echo hello
                 echo world 1>&2
                 echo foo
                 echo bar 1>&2")
(:RETURN-CODE 0 :OUT-LINES ("hello" "foo") :ERR-LINES ("world" "bar"))
```


## Background Commands

BOZO document me


## Stopping Shells

Shellpool does not provide a way to stop the shells after they have been
created.

It is vaguely possible that we could add a shutdown mechanism, but this would
be challenging to get right.  For instance, what if there are commands running
in the sub-shells?  Would you want to kill them all?
