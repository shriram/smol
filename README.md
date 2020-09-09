# The SMoL Language Family

This is the "smol" package for [Racket](https://racket-lang.org/).

SMoL is the Standard Model of Languages, a concept introduced
in the third edition of [PLAI](https://plai.org/).

## Installation

Install this package using the Racket packet manager:

* From DrRacket, go to File | Install Package, and enter the URL

  `https://github.com/shriram/smol.git`

* At the command line, run

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

## Documentation

You can read documentation in the Racket documentation for your
*local* installation (*not* on
[the Racket Web site](https://docs.racket-lang.org/)). In DrRacket, go
to Help | Racket Documentation. This will open a Web browser
window. In that window, you can either type `smol` or scroll down to
look for “SMoL” on that page.

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
[which version](https://github.com/shriram/smol/commits/master)
you currently have installed.

## Running from the Command Line

You can run files normally using the `racket` binary.

If you want a REPL in a SMoL language, say `smol/fun`, use this
command line:
```
racket -l smol/fun/semantics -i
```
(Replace `fun` with whatever other language you prefer.) This will
create an (interactive) REPL in that language.
