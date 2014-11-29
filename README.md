## Extremely preliminary, not ready for public consumption

Shellpool
=========

Shellpool is a way to run external programs from within a Common Lisp
program.  It features:

 - **Output streaming and capture**.  You can (optionally) have the program's
   output printed at runtime, but still (optionally) capture its output as a
   string list.  This makes it easy to not only (1) show a user a long-running
   sub-program's output as it's being created, but also (2) parse and analyze
   the program's output from your Lisp program.

 - **Exit code**.  Yep, you get it.

 - **Interruption**.  Interrupts are handled gracefully.  After you interrupt
   (e.g., Control C), you can `:continue` to keep running the program, or `:q`
   to send the sub-program a KILL signal.

 - **Forking**.  Sub-programs are launched with a separate shell, so you can
   avoid [forking](http://en.wikipedia.org/wiki/Fork_%28operating_system%29)
   your Lisp image, which may be unreliable when applications have dozens of GB
   of memory allocated or involve multiple threads.

 - **Background jobs**.  You can (optionally) launch external programs in the
   background.  (You don't get the exit codes and streaming features in this
   case.)

 - **Multithreading**.  Shellpool is safe for launching multiple programs from
   multiple threads.  Its multithreading support is based on the
   [bordeaux-threads](http://common-lisp.net/project/bordeaux-threads/)
   library.


## Documentation

 - [Installation](INSTALL.md)

 - [API Documentation](DOC.md)


## Portability

 - CCL (to start with)
 - other lisps in the future


## Comparison with related libraries

 - [inferior-shell](http://common-lisp.net/projects/qitab/) is allegedly very
   complete and portable for synchronous shells.  It has fancy features like
   support for remote execution (via ssh) and a domain specific language for
   constructing pipelines of shell commands.

 - [trivial-shell](http://common-lisp.net/project/trivial-shell/) is less full
   featured but apparently highly portable.

 - [ASDF](http://common-lisp.net/project/asdf/asdf.html) has `run-program`
   shell-command with many options.

 - [external-program](https://github.com/sellout/external-program) is a wrapper
   for `run-program` functionality.  It might be good to use this internally to
   run our shells and thereby avoid CCL-specific features.


## License and History

Shellpool is Copyright (C) 2014 [Kookamara LLC](http://www.kookamara.com/) and
released under an [MIT style license](LICENSE).

Shellpool is a successor to "tshell", a mechanism for running external programs
from [Clozure Common Lisp](http://ccl.clozure.com/).  Tshell was developed by
[Centaur Technology](http://www.centtech.com/) and was distributed as a library
for [ACL2](http://www.cs.utexas.edu/users/moore/acl2), and was also released
under the MIT license.
