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

@defmodulelang[smol/fun]

@(local-table-of-contents)

@section[#:tag "smol-fun"]{The @code{smol/fun} Language}

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

@defform[
(if test-expr then-expr else-expr)
]{
The @code{if} construct produces the value of either
@code{then-expr} or @code{else-expr}, depending on how
@code{test-expr} evaluates. In SMoL, only @code{false} is false;
all other values are true.
}

@deftogether[(
@defform[(ivec elem ...)]
@defform[(vlen vec-expr)]
@defform[(vref vec-expr idx-expr)]
)]{
The @code{ivec} operation builds an @emph{immutable} vector of the
elements in @code{elem}. Vector elements need not be of the same
type. @code{vlen} computes its length, while @code{vref} indexes
into it (starting from @code{0}).
}

@deftogether[(
@defform[(pair elem-1 elem-2)]
@defform[(left pair-expr)]
@defform[(right pair-expr)]
@defform[(pair? expr)]
)]{
@code{pair} is a special-case of @code{ivec}: it creates a
two-element vector. @code{left} and @code{right} access the left
(index @code{0}) and right (index @code{1})
elements. @code{pair?} recognizes @emph{any} two-element vector, not
only just those built using @code{pair}.
}

@defform[(++ str-expr ...)]{
@code{++} concatenates any number of strings.
}

@subsection[#:tag "debugging"]{Debugging}

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
	 @code{and}, @code{or}, @code{not},
	 @code{eq?}, @code{equal?},
	 @code{begin},
	 @code{+}, @code{-}, @code{*}, @code{/},
	 @code{zero?}, and
	 @code{string=?}
are all inherited directly from Racket and behave exactly as they do
there.
