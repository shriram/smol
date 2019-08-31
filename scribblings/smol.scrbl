#lang scribble/manual

@(require scribble/example)

@title{The SMoL Language Family}

The SMoL languages accompany the third edition of
@hyperlink["https://plai.org/"]{PLAI}.

There is a core set of shared semantic features in most widely-used
languages, ranging from Java and Python to Racket and OCaml to Swift
and JavaScript and beyond. Most contemporary mainstream programmers
program atop a language built atop it. That makes it worth
understanding.

SMoL, which stands for @emph{S}tandard @emph{M}odel @emph{o}f
@emph{L}anguages, embodies this common core. As the name suggests, it
also strips these languages to their essence. This aids understanding
by eliminating features that are either not universal or are only
incidental to understanding the core.

@(local-table-of-contents #:style 'immediate-only)

@section[#:tag "smol-fun"]{The @code{smol/fun} Language}

@defmodulelang[smol/fun]

@subsection[#:tag "definitions"]{Definitions}

@defform[
(defvar id expr)
]{
Defines a new identifier, @code{id}, and binds it to the value of
@code{expr}.
}

@defform[
(deffun (fun-id arg-id ...) expr)
]{
The @code{fun-id} is the name of the function; the remaining
@code{arg-id}s are its parameters. These are bound before evaluating
@code{expr}. @code{deffun} permits recursive definitions.
}

TODO: include

@subsection[#:tag "expressions"]{Expressions}

The base expression values are numbers, strings, symbols,
Booleans. The language also permits, but does not provide useful
operations to work with, list constants, vector constants, and more
exotic quoted forms. (If you don't know what these are, ignore them.)

@deftogether[(
@defproc[(ivec (elem Any) ...) Vec]
@defproc[(vlen (expr Vec)) Number]
@defproc[(vref (vec-expr Vec) (idx-expr Number)) Any]
)]{
The @code{ivec} operation builds an @emph{immutable} vector of the
elements in @code{elem}. Vector elements need not be of the same
type. @code{vlen} computes its length, while @code{vref} indexes
into it (starting from @code{0}).
}

@deftogether[(
@defproc[(pair (elem-l Any) (elem-r Any)) Vec]
@defproc[(left (expr Pair)) Any]
@defproc[(right (expr Pair)) Any]
@defproc[(pair? (expr Any)) Boolean]
)]{
@code{pair} is a special-case of @code{ivec}: it creates a
two-element vector. @code{left} and @code{right} access the left
(index @code{0}) and right (index @code{1})
elements. @code{pair?} recognizes @emph{any} two-element vector, not
only just those built using @code{pair}.
}

@defproc[(++ (s String) ...) String]{
@code{++} concatenates any number of strings.
}

@subsection{Applications}

Functions may not be passed as parameters.

@subsection[#:tag "debugging"]{Testing and Debugging}

Testing and debugging are intertwined. The more tests you write, the
less debugging work you will have to do. This is because tests
@emph{localize} debugging: if @code{f} calls @code{g} calls @code{h}
and the result of a call to @code{f} isn't what you expect, you have
no idea where the problem might lie. But if you have good tests for
some of these functions, then you have a fairly safe bet that the
problem is in the ones for which you don't. The more robustly you
test, the farther you push the boundary of trust, and the less effort
you have to later spend debugging.

The forms @code{test}, @code{test/pred}, and @code{test/exn} are all
available from @code{plai}. SMoL adds

@defform[
(test/not result-expr not-expected-expr)
]{
This is just like @code{test}, except the sense of equality is
inverted. Sometimes it's useful to write @emph{negative} tests: tests
that say a particular behavior will @emph{not} happen. For instance,
if you're testing scopes and have two variables with the same name but
different values, it's expressive to say that a particular value
(bound to the variable not in scope) will not show up.
}

@defform[
(spy expr)
]{
The @code{spy} construct is essentially ``@tt{printf} done right'',
especially for expression-oriented languages. It prints both the
source expression, source location, and resulting value of the
enclosed expression. It then returns that value. Note that any
@emph{expression} can be wrapped, not only a variable.

Therefore, at any point in the program, to study the value a
particular expression takes, just wrap it in @code{spy}. It
continues to produce a value, while the output shows both the source
expression (which is helpful if you have multiple @code{spy}s) as
well as the source location (in case you inspect multiple locations
that have the same source term).
}

@subsection[#:tag "racket"]{Inherited from Racket}

The constructs @code{trace}, @code{untrace},
	 @code{provide}, @code{all-defined-out},
         @code{let}, @code{let*}, @code{letrec},
	 @code{if}, @code{and}, @code{or}, @code{not},
	 @code{eq?}, @code{equal?},
	 @code{begin},
	 @code{+}, @code{-}, @code{*}, @code{/},
	 @code{zero?}, and
	 @code{string=?}
are all inherited directly from Racket and behave exactly as they do
there.

@section[#:tag "smol-state"]{The @code{smol/state} Language}

@defmodulelang[smol/state]

The @code{smol/state} language includes all of
@secref["smol-fun"], and the following in addition.

The @code{set!} construct from Racket, which changes the values that
variables are bound to.

The @code{begin} construct from Racket is also available, to sequence
operations.

@deftogether[(
@defproc[(mvec (elem Any) ...) Vec]
@defproc[(vset! (vec Vec) (idx Num) (val Any)) Void]
)]{
@code{mvec} creates @emph{mutable} vectors, and @code{vset!} modifies
them. @code{vset!} cannot modify an immutable vector.
}

@deftogether[(
@defproc[(mpair (elem-l Any) (elem-r Any)) Vec]
@defproc[(set-left! (expr Pair) (val Any)) Void]
@defproc[(set-right! (expr Pair) (val Any)) Void]
)]{
@code{mpair} creates @emph{mutable} pairs (which are just two-element
mutable vectors), and @code{set-left!} and @code{set-right!} modify
them. The elements are accessed using @code{left} and @code{right}, as
before.
}

@subsection{Applications}

Functions may not be passed as parameters.

@section[#:tag "smol-hof"]{The @code{smol/hof} Language}

@defmodulelang[smol/hof]

The @code{smol/hof} language includes all of
@secref["smol-state"] (with the exception of @secref["smol-hof-app"],
and the following in addition from Racket:
@itemlist[

@item{the constructs @code{lambda} and @code{λ} (which is just an
alias for @code{lambda}),}

@item{the list generators, @code{cons}, @code{empty}, and @code{list},
and}

@item{the functions @code{map}, @code{filter}, @code{foldl}, and
@code{foldr}.}

]

@subsection[#:tag "smol-hof-app"]{Applications}

Functions may be passed as parameters. This is the main point of this
language.

@section[#:tag "compat"]{Compatible Use in Racket}

If you want to program in some other language (typically
@code{racket}) and would like to use constructs defined in SMoL, you
can use the @code{compat} languages that are defined for each SMoL
level by appending @code{compat} to the language name. For instance,
@code{smol/fun/compat} is the compatibility layer for @code{smol/fun}.

As an example, these two programs behave exactly the same way:
@codeblock{
#lang smol/fun

(defvar x 3)
(++ "x" (spy (++ "y" "z" (++))))
}
and
@codeblock{
#lang racket

(require smol/fun/compat)

(defvar x 3)
(++ "x" (spy (++ "y" "z" (++))))
}
but the latter gives you access to all of the rest of Racket as
well. You could use compatibility layer because you find some of these
constructs more familiar, comfortable, or convenient than their
counterparts in Racket, but otherwise want to use Racket's more
powerful mechanisms (such as its macro system).

The compatibility layers provide @emph{only} the @emph{names} provided
by each of the languages; they do not provide any of the language
restrictions. Thus, for instance, if you import
@code{smol/fun/compat}, you can still use higher-order functions in
Racket as you normally would.

Warning: The @emph{intent} is that using this compatibility layer will
leave the behavior of programs unchanged. However, if you import these
bindings into a language with significantly different behavior than
Racket, what they do is undefined. It's safe to think of this as a
@emph{Racket} compatibility layer; it does not (nor can it) attempt to
preserve the semantics in all other languages. For example, @code{spy}
depends on being able to generate terminal output, but if the host
language forbids any output, then @code{spy} may also be compromised,
depending on how the host language has been implemented.
