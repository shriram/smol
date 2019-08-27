This is the "smol" package for [Racket](https://racket-lang.org/).

SMoL is the Standard Model of Languages, a concept introduced in the
third edition of [PLAI](https://plai.org/).

Install this package using the Racket packet manager:

* From DrRacket, go to File | Install Package, and enter the URL for
  this repository.

* At the command command line, run

  raco pkg install <URL for this repository>

  Make sure your paths are set correctly so that you're installing the
  package for the right version!

Once installed, you can test by going into DrRacket, creating a new
tab (if necessary, go to the Language menu and choose "The Racket
Language"). Then type
```
#lang smol/fun

(+ 1 2)
```
and run your program. You should see the output `3` in the REPL.
