## Extremely preliminary, not ready for public consumption

Shellpool
=========

Shellpool is a way to run external programs from within a Common Lisp
program.  It features output streaming and capture, exit code capture,
and better control over when forking occurs:

 - **Output streaming and capture**.  You can (optionally) have the
   program's output printed at runtime, but still (optionally) capture
   its output as a string list.  This makes it easy to show a user a
   long-running sub-program's output as it's being created, but also
   parse and analyze the program's output from your Lisp program.

 - **Exit code**.  Yep, you get it.

 - **Interruption**.  Interrupts are handled gracefully.  After you
   interrupt (e.g., Control C), you can @(':continue') to keep running
   the program, or @(':q') to send the sub-program a KILL signal.

 - **Forking**.  Sub-programs are launched with a separate shell, so
   you can avoid
   [forking](http://en.wikipedia.org/wiki/Fork_%28operating_system%29)
   your Lisp image.  (For applications that have dozens of GB of memory
   allocated, trying to fork can result in sudden, graceless death,
   despite the wonders of copy-on-write pages.)

 - **Background jobs**.  You can (optionally) launch external programs
   in the background.

 - **Multithreading**.  Shellpool is safe for launching multiple
   programs from multiple threads.  Its multithreading support is
   based on the
   [bordeaux-threads](http://common-lisp.net/project/bordeaux-threads/)
   library.


## Documentation

 - [Installation](INSTALL.md)

 - [API Documentation](DOC.md)


## Portability

 - CCL (to start with)
 - other lisps in the future


## License and History

Shellpool is Copyright (C) 2014 [Kookamara
LLC](http://www.kookamara.com/) and released under an [MIT style
license](LICENSE.md).

Shellpool is a successor to "tshell", a mechanism for running external
programs from [Clozure Common Lisp](http://ccl.clozure.com/).  Tshell
was developed by [Centaur Technology](http://www.centtech.com/) and
was distributed as a library for
[ACL2](http://www.cs.utexas.edu/users/moore/acl2), and was also
released under the MIT license.
