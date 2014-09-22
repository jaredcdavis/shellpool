Shellpool Installation
======================

Eventually I would like this to become an ordinary Quicklisp package,
but it is currently way too preliminary to try to distribute via
Quicklisp.  In the interim, here is how to install it.

* Install [Clozure Common Lisp](http://ccl.clozure.com/)

* Install [Quicklisp](http://www.quicklisp.org/), e.g.,:

```shell
$ curl -O http://beta.quicklisp.org/quicklisp.lisp
$ ccl
(load "quicklisp.lisp")
(quicklisp-quickstart:install)
(ql:add-to-init-file)
  ;; hit enter key
(quit)
```

* Locate your quicklisp `local-projects` directory,
  e.g., `~/quicklisp/local-projects`

* Use git to clone `shellpool` into local-projects/shellpool, e.g.,:

```shell
$ cd ~/quicklisp/local-projects
$ git clone https://github.com/jaredcdavis/shellpool
```

This should then allow you to load Shellpool by just quickloading it, e.g.,:

```
$ ccl
(ql:quickload "shellpool")
```
