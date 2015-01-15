Shellpool Tests
===============

These are tests that can be run to see if Shellpool seems to be working.

Usage:
```
ccl < top.lisp
```

 - On success, it should eventually "All tests passed."
 - On failure, it should exit with an error message.


These tests may be useful when porting Shellpool to other systems.  A quick
tour of the files here:

 - **basic.lisp** has tests of basic return code, opcode capture, and
   distinguishing between stdout/stderr.  It also has a few trivial attempts to
   \"break\" Shellpool (unbalanced parens, quotes, etc.)

 - **kill.lisp** has tests of graceful interruption.  It tries to ensure
   that subprograms can be killed and that Shellpool is still functional
   after killing has occurred.








