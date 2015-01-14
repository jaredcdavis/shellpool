## Extremely preliminary, not ready for public consumption

Shellpool
=========

Shellpool is a way to run external programs from within a Common Lisp
program.  It features:

 - **Output handling**.  You can provide a callback function that handles each
   line of output.  This makes it easy to stream, filter, or collect output as
   you like.  Your callback is invoked for both standard error and output
   lines, and can tell the difference.

 - **Exit code**.  Yep, you get it.

 - **Interruption**.  Interrupts are handled gracefully.  After you interrupt
   (e.g., Control C), you can `:continue` to keep running the program, or `:q`
   to send the sub-program a KILL signal.

 - **Forking**.  Sub-programs are launched with a separate shell, so you can
   avoid [forking](http://en.wikipedia.org/wiki/Fork_%28operating_system%29)
   your Lisp image, which may be unreliable when applications have dozens of GB
   of memory allocated or involve multiple threads.

 - **Background jobs**.  You can (optionally) launch external programs in the
   background.  (However, you don't get the exit codes and streaming features
   in this case.)

 - **Multithreading**.  You can safely launch multiple programs from multiple
   threads.  Threading support is based on the
   [bordeaux-threads](http://common-lisp.net/project/bordeaux-threads/)
   library.

Note however that Shellpool is **not** suitable for running sub-commands that
need access to a real tty terminal or even command-line input from the user.


## Documentation

 - [Installation](INSTALL.md)

 - [API Documentation](DOC.md)

 - Please use the [Github issue
   tracker](https://github.com/jaredcdavis/shellpool/issues) for any bugs or
   enhancement requests.


## Portability

Lisps:

 - CCL (to start with)
 - SBCL (not yet, but will be done for 0.1)

Operating Systems:

 - Linux, BSD, etc. (to start with)

I would welcome any patches that provide support for other Lisps or for Windows
environments.


### Dependencies

Lisp Libraries:

  - [cl-fad](http://weitz.de/cl-fad/) for handling temporary files
  - [bordeaux-threads](http://common-lisp.net/project/bordeaux-threads/) for multithreading
  - [bt-semaphore](https://github.com/rmoritz/bt-semaphore) for semaphores

Utilities:

  - [https://www.gnu.org/software/bash/](Bash)
  - [https://www.gnu.org/software/sed/](Sed)


## Related Lisp Libraries

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

Shellpool is Copyright (C) 2014-2015 [Kookamara LLC](http://www.kookamara.com/)
and released under an [MIT style license](LICENSE).

Shellpool is a successor to "tshell", a mechanism for running external programs
from [Clozure Common Lisp](http://ccl.clozure.com/).  Tshell was developed by
[Centaur Technology](http://www.centtech.com/) and was distributed as a library
for [ACL2](http://www.cs.utexas.edu/users/moore/acl2), and was also released
under the MIT license.
