# The SMoL Language Family

This is the "smol" package for [Racket](https://racket-lang.org/).

SMoL is the _S_tandard _M_odel _o_f _L_anguages, a concept introduced
in the third edition of [PLAI](https://plai.org/).

## Installation

Install this package using the Racket packet manager:

* From DrRacket, go to File | Install Package, and enter the URL

  `https://github.com/shriram/smol.git`

* At the command command line, run

  `raco pkg install https://github.com/shriram/smol.git`

  Make sure your paths are set correctly so that you're installing the
  package for the right version!

## Checking the Install

Once installed, you can test by going into DrRacket, creating a new
tab (if necessary, go to the Language menu and choose "The Racket
Language"). Then type
```
#lang smol/fun

(+ 1 2)
```
and run your program. You should see the output `3` in the REPL.

## Updating

If you're asked to _update_ your package, you can do it through

* the DrRacket package manager: File | Package Manager, then click on
  the package, and click on Update

* the command line: run

  `raco pkg update https://github.com/shriram/smol.git`

You can always find the current version of your package through

* the DrRacket package manager: File | Package Manager, then look at
  the package

* the command line: run

  `raco pkg show smol`

In either case, you should see a Checksum that looks something like
`812c8bbd` and so on. Compare that this begins the same way as the
checksum right after "Latest commit" earlier on this page. That tells
you that you have the most recent version of the package; otherwise,
it needs updating. Specifically, the checksum tells you
(which version)[https://github.com/shriram/smol/commits/master]
you currently have installed.
