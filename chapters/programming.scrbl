#lang scribble/manual
@(require "../rosetta.rkt")
@setup-math


@title{Introduction}

Knowledge transmission is one of the issues that has worried mankind
through the ages. As humans gather knowledge throughout their lives,
 it would be unfortunate if all that knowledge disappeared with their death.

To avoid this loss, mankind invented a series of mechanisms to
preserve knowledge. Firstly, oral transmission, consisting in the
transmission of knowledge from one person to a small group of people, thus
transferring the problem of knowledge preservation to the next
generation.  Secondly, written transmission, consisting in documenting
knowledge.  On one hand, this approach has the great advantage of
reaching out to a much larger group of people. On the other hand, it
significantly reduces the loss of knowledge due to transmission
problems. In fact, the written word allows to preserve knowledge for
long periods of time without the inevitable changes that occur on
a long chain of oral transmissions.

It is thanks to the written word that mankind can understand and
accumulate vast amounts of knowledge, some of it dating back to
thousands of years. Unfortunately, the written word has not always
been able to accurately transmit what the author had in mind: the
natural language is ambiguous and it evolves with time, making the
interpretation of written texts a subjective task. Whether we're
writing, reading, or interpreting a text, there are omissions,
imprecisions, errors, and ambiguities which can turn the knowledge
transmission fallible. If the transmitted knowledge is simple, the
receptor will most likely have enough culture and imagination to
understand it. However, the transmission of more complex knowledge might 
be much more difficult to accomplish.

When the transmission of knowledge requires rigour, relying on the
receptor's abilities to understand it can have disastrous outcomes
and, in fact, throughout history we can find many catastrophic events
caused solely by insufficient or incorrect transmission of knowledge.

To avoid these problems, more accurate languages were developed.
In particular, for the past millennia, Mathematics has obsessively 
sought to construct a language that shines for its absolute rigour. 
This allows knowledge transmission to be much more accurate
than in other areas, reducing to the bare minimum the need for
imagination in order to understand that knowledge.

To better understand this problem, let us consider one concrete example of
knowledge transmission: the calculus of the factorial of a number. If
we assume that the person, to whom we want to transmit that knowledge,
knows beforehand about numbers and arithmetic operations, we could tell him
that, @emph{to calculate the factorial of any number, one
must multiply every number from one until that number}.
Unfortunately, that description is too long and even worse inaccurate,
because it does not state that only integer numbers are to be
multiplied. To avoid these imprecisions and simultaneously make the
information more compact, Mathematics developed a set of symbols and
concepts that should be understood by everyone. For example, to define
the integer sequence of numbers between @${1} and @${9}, mathematics
allows us to write @${1,2,3,\ldots,9}. In the same manner, instead of
referring to "@emph{any number}", mathematics invented the concept of
"@emph{variable}": a name that refers to some "thing" that can be used in
several parts of a mathematical statement, always representing the
same "thing". That way, Mathematics allows us to more
accurately express the factorial computation as follows:

@$${n! = 1\times 2\times 3\times \cdots{} \times n}

But is this definition rigorous enough? Is it possible to interpret it
without requiring imagination to figure the author's intention?
Apparently, it is but there is a detail in this definition that
@emph{requires} imagination: the ellipses points. The ellipses indicate that the
the reader must imagine what should be in its place. Although most readers
will correctly understand that the author meant the multiplication of the sequential
numbers, some might think to replace the ellipsis with something
else.

Even if we exclude this last group of people from our target audience,
there are still other problems with the previous definition. Let us imagine,
for example, the factorial of @${2}. What is its value? If we use @$${n = 2} 
in the formula, we get:

@$${2! = 1\times 2\times 3\times \cdots{} \times 2}

In this case, the computation makes no sense, which shows that, in
fact, for more than just on how to fill in the ellipsis points, imagination is 
required: to realize that the number of terms to consider depends on the number 
to which we want to calculate the factorial. 

Assuming that our reader has enough imagination to figure out
this subtlety, he would easily calculate that @${2!=1\times
2=2}. But even then, there will be cases where it is not that clear.
For example, what is the factorial of @${0}? The answer does not
appear to be obvious. What about the factorial of @${-1} or the factorial of 
@${4.5}?. Again, the answer to these questions is unclear as the formula says 
nothing regarding these situations and our imagination can not guess the correct 
procedure.

Would it be possible to find a way of transmitting the knowledge
required to compute the factorial function that minimizes
imprecisions, gaps, and ambiguities? Let us try the following
variation of the definition of the factorial function:

@$${n!= 
\begin{cases}
1, & \text{if $n=0$}\\
n \cdot (n-1)!, & \text{if $n\in \mathbb{N}$.}
\end{cases}}

Have we reached the necessary rigour that requires no imagination on the
reader's part? One way to find out is to consider the previous cases that gave 
us problems. Fist of all, there are no ellipsis points, which is positive. 
Secondly, for the factorial of the number @${2}, we will have:

@$${2!=2\times 1!=2\times (1\times 0!)=2\times (1\times 1)=2\times 1=2}

which means that there is no ambiguity. Finally, we can see that it
makes no sense trying to determine the factorial value of
@${-1} or @${4.5} because this function can only be
applied to @${\mathbb{N}_0} members}. 

This example shows that, even in mathematics, there are different levels
of rigour in the different ways that is possible to transmit knowledge.
Some require more imagination than others but, in general, they have been
enough for mankind to preserve knowledge throughout history.

Nowadays mankind has a partner that has been making valuable contributions to 
the progress of knowledge preservation: @emph{the computer}. This machine has
an extraordinary capability: given a set of instructions, it knows how to 
execute a complex set of tasks. Programming is essentially all about 
transmitting to a computer the knowledge needed to solve a specific problem. 
This knowledge is called a @emph{program}. Due to computers being 
@emph{programmable}, they have been used for various purposes, and for the last 
decades they have radically changed the way we work.
Unfortunately, the computer's extraordinary ability to learn comes with an 
equal extraordinary lack of imagination. A computer does not assume or imagine, 
it rigorously interprets the knowledge that is transmitted in the form of a 
program.

Since it has no imagination the computer critically depends on how
we present the knowledge that we wish to transmit: that knowledge description 
must not be ambiguous, nor contain gaps, nor imprecisions. A language
with these characteristics is generally called a @emph{programming language}.


@section{Programming Languages}

For a computer to be able to solve a problem it is necessary to
describe the process of solving a problem in a language that it
understands. Unfortunately, the language that a computer "innately" understands
is extremely poor, making the description of how to solve a non-trivial
problem a very exhausting, tedious and complex task. By introducing linguistic 
elements capable of simplifying those descriptions, countless programming 
languages have managed to reduce the programmer's effort. For example, the
concept of @emph{function}, @emph{sum}, @emph{matrix} or @emph{rational number} 
do not exist natively in computers but many programming languages allow their 
usage in order to simplify the description of scientific calculus.
Naturally, there must be a process that is able to transform the programmer's
descriptions into instructions that computers can understand. In spite of being 
relatively complex, this process allows us to have programming languages that
operate closer to human thinking process rather than the computer's.

This last fact is of extreme importance because it allows us to use programming
languages not only to instruct a computer on how to solve a problem, but also
to accurately explain that process to another human being. Hence, 
just as mathematics has been for the last thousands of years, programming 
languages become a way of transmitting knowledge.

There is a huge amount of programming languages, some better equipped 
than others to solve specific problems. The choice of a programming language should
therefore depend heavily on the type of the problems we wish to solve but it should not be a total
commitment. For a programmer it is much more important to understand the
fundamentals and techniques of programming than to master this or that
language. However, to better understand these fundamentals, it's convenient to
exemplify them in a concrete programming language.

As the focus of this document will be on programming for Architecture,
we will use a programming language that is geared towards solving geometrical
problems. There are many languages that serve this purpose,
most commonly associated with computed aided design tools - @emph{Computer Aided
Design} (CAD). ArchiCAD, for instance, offers a programming language
called GDL, an acronym for @emph{Geometric Description Language} that enables users
to program multiple geometric forms. In the case of AutoCAD, it uses a language called
AutoLisp, a dialect of a famous programming language called Lisp. A third
option will be the RhinoScript language, available for
Rhinoceros. Despite these languages seeming very different from each
other, the concepts behind them are very similar. It is on these concepts that we
will be leaning on, although for pedagogical reasons, it is convenient
to particularize them in a single language.

Unfortunately GDL, AutoLisp, and RhinoScript were developed a long
time ago and they have not been updated, possessing many archaic
characteristics that makes them harder to learn and use. In order to
ease the learning process and simultaneously allow our programs
to run in different CAD environments, we are going to use a new
language called Racket, that has been intentionally adapted for programming in
Architecture. In this text, we will explain the fundamentals of programming using Racket,
not only because it's easier to learn, but also for it's practical applicability.
However, once learned, the reader should be able to apply these fundamentals
to any other programming language.

In order to facilitate the programmer's task, Racket is equipped with
a programming environment called DrRacket, that offers a text editor
adapted to edit Racket's programs, as well as a set of additional
tools for error detection and debugging. This programming
environment is shared with a freeware license and it is available at:
@;{\verb|http://racket-lang.org/|}


@questions[
@question{
Exponentiation @${b^n}is an operation between two numbers @${b} and
@${n}. When @${n} is a positive integer, exponentiation is defined as:

@$${b^n = \underbrace{b \times b \times \cdots \times b}_n}

To a reader not familiarized with exponentiation, the previous
definition raises several questions that may not be evident: how many multiplications should actually
be done?,@${n}?, @${n-1}? What if @${b=1}? or @${b=0}?  Suggest a
definition for the exponentiation function in order to clear up any
doubts.}

@question{What is a program and what purpose does it serve?}

@question{What is a programming language and what purpose does it serve?}
]

@section{The Racket Language}

In this section we will learn about Racket programming language, which we will use throughout
this text. But first, we are going to examine some aspects that are common to other languages.

@subsection{Syntax, Semantics and Pragmatics}

Every language has @emph{syntax}, @emph{semantics} and
@emph{pragmatics}.

In simple terms, syntax is a set of rules that dictate
the kind of sentence that can be written in that language. Without it, any
concatenation of words could be a sentence. For example, given the words
“John”, “cake”, “ate”, “the” the syntax rules of the English language tell
us that - “John ate the cake” is a correct sentence, and that - “ate the John
cake” is not. Note that according to the English syntax, "The cake ate John"
is also syntactically correct.

Syntax dictates how a sentence is constructed but says nothing in regards
to its meaning. Semantics associate meaning to a sentence, thus
telling us that “The cake ate John” makes no sense.

Finally, pragmatics set the way sentences are commonly expressed. In
a language, pragmatic changes depending on the context: the way two close
friends talk with each other is different from the way two strangers talk.

These three aspects of a language are also present when we discuss
programming languages. Unlike the @emph{natural} languages we use to
communicate between us, programming languages are characterized as
being @emph{formal}, obeying a set of simple and restrictive rules
that can be @emph{mechanically} processed.

In this document we will describe Racket's syntax and semantics and, although
there are mathematical formalisms to describe rigorously these two aspects, they
require a mathematical sophistication that, given the nature of this work, is
inappropriate and hence we will only use informal descriptions.  Afterwards, as we
introduce language elements, we will discuss language pragmatics.

@subsection{Syntax and Semantics of Racket}

When compared to other programming languages, Racket's syntax is
extraordinary simple and is based on the concept of @emph{expressions}.

In Racket, an expression can be formed either by primitive elements such as numbers,
or by the combination of those elements, such as the sum of two numbers.
This simple definition allows us to build expressions of arbitrary complexity.
However, it is important to remember that syntax restricts what can be written:
the fact that we can combine expressions to create more complex ones, does
not mean that we can write @emph{any} combination of sub-expressions. These combinations
are restricted by syntactic rules that we will describe throughout this text.

Much like the syntax, Racket's semantics is also very simple when compared to other
programming languages. As we will see, semantics is determined by the @emph{operators}
that are used in our expressions. For instance, the sum operator is used to add two
numbers. An expression that combines this operator with, for example, the numbers @${3} and
@${4} will have as its meaning the sum between @${3} and @${4}, i.e., @${7}.
In a programming language, the semantics of an expression is given by the computer that
will @emph{evaluate} the expression.

@subsection{The Evaluator}

Every expression in Racket has a value. This concept is so important
that Racket provides an evaluator, i.e., a program designed to interact
with the user in order to evaluate expressions defined by the user.

In Racket, the evaluator is shown as soon as we start working with the DrRacket
environment, and it is possible to easily change between the editor and the
evaluator at any time. Once DrRacket is running, the user is presented with
the character @verb{>} (called @emph{prompt}), meaning that Racket is waiting for
the user to input an expression.

User's expressions will be shown in front of Racket's "prompt" character ">". 
Racket interacts with the user by executing a cycle that reads 
expressions, determines its value and writes the result. This cycle is
traditionally called @emph{read-eval-print-loop} (abbreviated to REPL).

During the read phase, Racket reads an expression and creates an internal object
that represents it. In the evaluation phase, that object is analysed in order to
produce a value. This analysis uses rules that dictate, for each case, the object's
value. Finally, at the print phase, the result is returned to the user in text 
format.

Given the existence of the @emph{read-eval-print-loop} process in Racket, it is not
necessary to instruct the computer to explicitly print the result of an expression,
which means that testing and debugging is significantly easy. The advantage of Racket being
an interactive language is that it allows programs to be quickly developed by writing,
testing and correcting small fragments at a time.

@section{Language Elements}

In every programming language we have to deal with two sets of objects: data and
procedures. Data comprise all the entities that we wish to manipulate. Procedures
designate the rules that specify how to manipulate that data.

In Mathematics, we can look at numbers as the data and algebraic operations as the
procedures. These operations allows us to @emph{combine} numbers. For example,
@${2\times 2} is a combination. Another combination involving more data is
@${2\times 2\times 2}, and using even more data @${2\times 2\times 2\times 2}.
However, unless we want to spend time solving problems of elementary arithmetic, we
should consider more elaborate operations that represent calculation patterns. In the
sequence of combinations previously shown, it is clear that the pattern that is emerging
is the definition of exponentiation, which has been defined in Mathematics a long time
ago. Therefore, exponentiation is an abstraction of a succession of multiplications.

As in Mathematics, a programming language should contain primitive data and procedures,
it should be capable of combining data and procedures to create more complex data and
procedures and it should be able to abstract calculation patterns and allow them to be
used as simple operations, defining new operations that represent those patterns.

Further ahead, we are going to see how it is possible to define these abstractions in Racket. 
For the time being, let us take a closer look at the @emph{primitive elements} of the language, i.e.,
the most simple entities that the language deals with.

@subsection{Numbers}

As previously said, Racket executes a @emph{read-eval-print} cycle. This implies that
everything we write in Racket must be evaluated, i.e., everything must have a value that Racket
displays on the @emph{screen}.

That way, if we give the evaluator a number, it will return that number's value. How much is the
value of a number? At best we can say that the number has its own value. For example, the value of number 1 is 1.

@incremental[1 12345 1/2 1+2i 4.5]

In Racket, numbers can be @emph{exact} or @emph{inexact}. Exact numbers include integers,
fractions and complex numbers with integer parts. Inexact numbers are all others, which are typically
written in decimal or scientific notation.

@section{Combinations}

A combination is an expression that describes the application of an operator to its operands. In
Mathematics, numbers can be combined using operations like sum or multiplication; e.g. @${1 + 2}
and @${1 + 2 \times 3}. Sum and multiplication of numbers are simply two of the primitive
procedures provided by Racket.

In Racket, a combination can be created by writing a sequence of expressions inside a pair of
parentheses. An expression is a primitive element or another combination. The expression
@lisp[(+ 1 2)] is a combination of two primitive elements @lisp[1] and @lisp[2] through the primitive
procedure @fn[+]. In the case of @lisp[(+ 1 (* 2 3))] the combination is between @${1} and @lisp[(* 2 3)]
(where @lisp[(* 2 3)] is also a combination). Note that each expression must be separated from the
rest using at least one space. Despite the combination @lisp[(* 2 3)] having three expressions - @fn[*],
@lisp[2] and @lisp[3], the combination @lisp[(*2 3)] only has two - @lisp[*2] and @lisp[3], in which the
first expression (@lisp[*2]) has no predefined meaning.

For now, the only useful combinations are those whose expressions have meaning as operators and operands.
Conventionally, Racket considers the first element of the combination as the operator and the rest its
operands.

The notation used by Racket to build expressions (the operator before the operands) is called prefix
notation. This form of notation may cause some perplexity to new users of this language since most of them
expect a notation closer to that taught in arithmetic and which is usually used in other programming languages. 
The expression @lisp[(+ 1 (* 2 3))] is normally written as @pascal{1 + 2 * 3} (designated infix notation). 
Although the infix notation (whose operator is placed between operands) is usually easier to read by a human, the prefix
notation used by Racket has advantages over the former, such as:

@itemlist[ 

@item{It is very easy to use @emph{variadic} operators, that is, operators
that have a variable number of operands, such as @lisp[(+ 1 2 3)] or
@lisp[(+ 1 2 3 4 5 6 7 8 9)]. Most programming languages use only
unary or binary operators. In such languages, binary operators must be written
between each two operands, e.g.: @pascal{1 + 2 + 3} or @pascal{1 +
2 + 3 + 4 + 5 + 6 + 7 + 8 + 9}. Note that if you intend to apply a ternary operator, 
you can't write it the same way as the unary or binary operator.}

@item{There are no precedences between operators. In languages that
use infix notation, expressions like @pascal{1 + 2 * 3}, must be disambiguated 
through precedence rules. These precedence rules  can be modified by parenthesis. 
Thus, the former expression @pascal{1 + 2 * 3} should be calculated as if it was 
written @pascal{1 + (2 * 3)} instead of @pascal{(1 + 2) * 3}.  In Racket, expressions 
like @lisp[(+ 1 (* 2 3))] or @lisp[(* (+ 1 2) 3)] would be necessarily different, 
which avoid any kind of ambiguity.}

@item{The operators we define are used exactly the same
way as the operators provided by the language: first the operator and
then the operands. In most other languages, operators are infix
(between operands) and procedures created by the user are prefix
(first the procedure's name and then the operands). This compromises
the extension of the language in a coherent way. To exemplify this last case,
let's consider the exponentiation operation in a language with infix notation.
To be coherent with the rest of the language there should be an operator, for
example @pascal{**}, that lets us write @pascal{3**4} to calculate 3 to the
fourth power. Since this operator usually does not exist, we are forced to create a
procedure that implements it, but in this case the syntax changes radically because,
in general, the user can not create infix operators. As a consequence, this new
operand would have to be used in a prefix way and thus not remain coherent with other
operators of the language, such as sum or multiplication. On the contrary, in
Racket we can either write @lisp[(* 3 3 3 3)] or create a function that allows us
to write @lisp[(** 3 4)].}]

Besides the infix and prefix notations, there is also the postfix notation in which
the operator comes after the operands. This notation has the same properties as the
prefix notation but it is, in general, more difficult to read since it is necessary
to read every operand before we are able to understand what should be done with them.

@subsection{Indentation}

The disadvantage of the prefix notation is the writing of complex combinations. The
expression @pascal{1+2*3-4/5*6} is easy to read but when written using Racket's syntax:
@lisp[(- (+ 1 (* 2 3)) (* (/ 4 5) 6))], it has a form that, for those not yet accustomed
to the syntax, might be harder to read due to the large number of parentheses.

To make the expression easier to read, we can (and we should) use
@emph{indentation}. This technique is based on using different
alignments in the textual disposition of programs in order to make
them easier to read. This way, instead of writing our expressions
in a single line or with an arbitrary line arrangement, we write them
throughout several lines, while using an alignment between lines that
shows how the sub-expressions are related to the expression that
contains them.

The rule for indentation in Racket is extremely simple: in one line we
have the operator and the first operand, the remaining operands are placed
immediately below the first with enough blank spaces on the left so that they
are correctly aligned. In the case of a short expression, we can write it
in a single line, with the operands immediately after the operator, using a
single blank space to separate them. Using these two rules, we can rewrite the
previous expression in the following manner:

@lispcode[
(- (+ 1
      (* 2 3))
   (* (/ 4 5)
      6))]

@;{\newcommand{\indentruleA}{\begin{tikzpicture}\draw[use as bounding box];\draw(0,0.2)--(0,-0.9);\end{tikzpicture}}
\newcommand{\indentruleB}{\begin{tikzpicture}\draw[use as bounding box];\draw(0,0.2)--(0,-0.4);\end{tikzpicture}}
\begin{lispcode}
(- \indentruleA(+ \indentruleB1
      (* 2 3))
   (* \indentruleB(/ 4 5)
      6))
\end{lispcode}}

Note that arranging a combination in several lines does not affect the way Racket reads it.
The correct delimitation of elements in a combination is done only by the visual separation
and the correct usage of stacked parenthesis.

When the indentation rule is not enough to produce an aesthetically pleasing disposition of textual lines,
some minor variations may be used, such as placing the operator in a line and the operands underneath it,
like in the following example:

@lispcode[
(an-operator-with-a-very-very-very-big-name 
  1 2 3 4)]

Generally, we might need to apply several rules simultaneously:

@lispcode[
(an-operator (an-operator-with-a-very-very-very-big-name
               1 2 3 4)
             (another-operator 5
                               6
                               (and-another 7
                                            8))
             (and-the-last-one 9 10))]

@;{\newcommand{\indentruleC}{\begin{tikzpicture}\draw[use as bounding box];\draw(0,0.2)--(0,-2.5);\end{tikzpicture}}
\newcommand{\indentruleD}{\begin{tikzpicture}\draw[use as bounding box];\draw(0,0.2)--(0,-0.9);\end{tikzpicture}}
\begin{lispcode}
(an-operator \indentruleC(an-operator-with-a-very-very-very-big-name
               1 2 3 4)
             (another-operator \indentruleD5
                             6
                             (and-another  \indentruleB7
                                            8))
             (and-the-last-one 9 10))
\end{lispcode}}

Some operators have a specific indentation rule but this will be explained as we introduce them. 

Indentation is crucial in Racket because it makes it easier to write and read complex code.
Most editors that recognize Racket's language automatically format the programs as we write them, 
also showing the correct matching between parenthesis. Eventually with practice, it becomes
easier to write and read programs, regardless of how complex their structure is.

@questions[
@question{Define REPL?}
@question{What is the difference between prefix and infix notations?}
@question{In Mathematics it is usual to use simultaneously the prefix, infix and postfix notations. Write some mathematical examples of expressions that make use of these different notations.}
@question{Convert the following arithmetic infix expressions to Racket's prefix notation:
@itemlist[#:style 'ordered 
@item{@${1 + 2 - 3}}
@item{@${1 - 2 \times 3}}
@item{@${1 \times 2 - 3}}
@item{@${1 \times 2 \times 3}}
@item{@${(1 - 2) \times 3}}
@item{@${(1 - 2) + 3}}
@item{@${1 - (2 + 3)}}
@item{@${2 \times 2 + 3 \times 3 \times 3}}]
}

@question{Convert the following Racket prefix notation expressions into arithmetic infix ones:
@itemlist[#:style 'ordered 
@item{@lisp[(* (/ 1 2) 3)]}
@item{@lisp[(* 1 (- 2 3))]}
@item{@lisp[(/ (+ 1 2) 3)]}
@item{@lisp[(/ (/ 1 2) 3)]}
@item{@lisp[(/ 1 (/ 2 3))]}
@item{@lisp[(- (- 1 2) 3)]}
@item{@lisp[(- 1 2 3)]}]
}

@question{Use indentation to rewrite the following expression, so that there is only a single operand per line.

@lisp[(* (+ (/ 3 2) (- (* (/ 5 2) 3) 1) (- 3 2)) 2)]}
]

@subsection{Evaluating Combinations}

As we have seen, Racket considers the first element of a combination as the operator and
the rest as operands.

The evaluator determines the combination's value by applying the procedure specified by the user
to the value of the operands. The value of an operand is called the argument of the
procedure. The value of the combination @lisp[(+ 1 (* 2 3))] is the result of adding the value of 1 to
@lisp[(* 2 3)]. As we have already seen, the value of @lisp[1] is @${1} and @lisp[(* 2 3)] is a
combination whose value is the result of multiplying @lisp[2] by @lisp[3], which is @${6}.
Finally, by summing @${1} with @${6} we get @${7}.

@incremental[(* 2 3) (+ 1 (* 2 3))]

@question{Calculate the value of the following Racket expressions:

@itemlist[#:style 'ordered 
@item{@lisp[(* (/ 1 2) 3)]}
@item{@lisp[(* 1 (- 2 3))]}
@item{@lisp[(/ (+ 1 2) 3)]}
@item{@lisp[(- (- 1 2) 3)]}
@item{@lisp[(- 1 2 3)]}
@item{@lisp[(- 1)]}]
}

@subsection{Strings}

@emph{Chains of characters} (also called @emph{Strings}) are another type of primitive data. A character is a
letter, a digit or any kind of graphic symbols, including non-visible graphic symbols like blank
spaces, tabs and others. A string is specified by a character sequence between quotations marks. Just
like with numbers, the value of a string is the string itself:

@incremental["Hi" "I am my own value"]

Since a @emph{string} is delimited by quotation marks, one could ask how can we create a string that
contains quotation marks. For this and other special characters, there is a special character that Racket
interprets differently: when in a strings the character @verb{\} appears it tells Racket that the next characters
must be evaluated in a special way. For example, to create the following @emph{string}:

@quote{
  John said "Good morning!" to Peter.}

We must write:

@lisp["John said \"Good morning!\" to Peter."]

@table[#:tag "tab:caracteresEscape" #:caption @elem{Some valid @emph{escape} characters in Racket.}
@tabular[#:style 'boxed
	 #:sep @hspace[1]
         #:column-properties '(left left)
         #:row-properties '(bottom-border ())
         (list (list @bold{Sequence}@bold{Result})
	       (list @literal|{\\}| @elem{the character @literal|{\}| (@emph{backslash})})
	       (list @literal|{\"}| @elem{the character @literal|{"}| (quotation marks)})
	       (list @literal|{\e}| @elem{the character @emph{escape}})
	       (list @literal|{\n}| @elem{new line character (@emph{newline})})
	       (list @literal|{\r}| @elem{new line character (@emph{carriage return})})
	       (list @literal|{\t}| @elem{tab character (@emph{tab})}))]
]

The character @verb{\} is called an @emph{escape} character and allows the inclusion of characters
in strings that would otherwise be difficult to input. @tabref{tab:caracteresEscape}) shows examples
of other @emph{escape} characters.

As it happens with numbers, there are countless operators to
manipulate @emph{strings}. For example, to concatenate multiple
@emph{strings} there is the @lisp[string-append] operator. The
concatenation of several strings produces a single string with
all characters of those strings, in the same order:

@incremental[
(string-append "1" "2")
(string-append "one" "two" "three" "four")
(string-append "I" " " "am" " " "a" " " "string")
(string-append "And I" " am " "another")
]

To know how many characters there are in a string we have the @lisp[string-length] operator:

@incremental[
(string-length "I am string")
(string-length "")
]

Note that quotation marks define strings' boundaries and are not
consider characters.  Besides @emph{strings} and numbers, Racket has
other kinds of primitive elements that will be addressed later.

@section[#:tag "sec:syntaxDefine"]{Defining Functions}

In addition to basic arithmetic operations, Mathematics offers a large
set of operations that are defined based on those basic ones. For
example, the @emph{square} of a number is an operation (also
designated as a @emph{function}) that, given a number, calculates 
the multiplication of that number by itself. This function has the following
mathematical definition @${x^2 = x \cdot x}.

Like in Mathematics, it is possible to define the square function in a programming
language. In Racket, to obtain the square of a number, for example @lisp[5], we
write the combination @lisp[(* 5 5)]. In general, given a number @lispf{x}, we know
to obtain the square value by writing @lispf{(* x x)} combination. All that remains now
is to associate a name indicating that, given a number @lispf{x}, we
obtain its square by evaluating @lispf{(* x x)}. Racket allows us to do that by
using the @stx[define] operation:

@def[
(define (square x) (* x x))
]

As you can see from the @lisp[square] function definition, in order to
define a function in Racket, we need to combine three elements. The
first element is the word @stx[define], that informs the evaluator that
we are defining a function. The second element is a combination
with the function's name and its parameters. The third element is the
expression that will compute the function value for those
parameters. In generic terms we could say that the definition of
functions is done in the following manner:

@lispcode[
(define (_name #,(lispemphi parameter "1") ... #,(lispemphi parameter "n"))
  _body)]

The function's parameters are called @emph{formal parameters} and they
are used in the body of an expression to refer to the correspondent
arguments. When we write in the evaluator @lisp[(square 5)], the
number @lisp[5] is the formal parameter. During the calculation this argument
is associated with the number @lispf{x}. The arguments of a function are also
called @emph{actual parameters}.

The definition of the @lisp[square] function declares that in order to determine the
square of a number @lispf{x}, we should multiply that number by itself @lispf{(* x x)}.
This definition associates the word @emph{square} with a procedure, i.e., a description on
how to obtain the desired result. Note that this procedure has parameters allowing it
to use different arguments. For example, let's evaluate the following expressions:

@incremental[
(square 5)
(square 6)
]

The rule to evaluate combinations stated above is also valid for functions defined by the
user. The evaluation of the expression @lisp[(square (+ 1 2))] first evaluates @lisp[(+ 1 2)]
operand. This value of this operand, @${3}, is used by the function in place of @lisp[x].
The function's body is then evaluated and all occurrences of @lisp[x] will be replaced by the value
@lisp[3], i.e., the final value will b e the combination @lisp[(* 3 3 )].

Formally, in order to invoke a function, is necessary to construct a combination in which the first
element is an expression that evaluates for the function itself, and the remaining elements are
expressions that evaluate for the arguments that the function is supposed to use. The result of the
combination's evaluation is the value calculated by the function for those arguments.

The process of evaluating a combination is done by the following steps:

@itemlist[#:style 'ordered
@item{All elements in a combination are evaluated, with the value of the first one being
necessarily a function.}
@item{The formal parameters are associated with the function's arguments, i.e., the value
of the remaining elements of that combination. Each parameter is associated to an argument,
according to the order of parameters and arguments. An error occurs when the number of parameters
and arguments is different.}
@item{The function's body is evaluated keeping in mind these associations between parameters and
arguments.}]

To better understand this process, it is useful to decompose it in its most elementary steps. The
following example shows the process of evaluating the expression @${((1+2)^2)^2} step by step:

@evaluation-steps[
(square (square (+ 1 2)))
(square (square 3))
(square (* 3 3))
(square 9)
(* 9 9)
81
]

Every function created by the user is evaluated by Racket in equal terms as other 
predefined function. This allows them to be used to create new functions. For example, 
after defining the @fn[square] function, we can define the function that calculates the
area of a circle with radius @${r}, using the formula @${\pi * r^2}:

@def[
(define (circle-area radius)
  (* pi (square radius)))
]

Naturally, during the evaluation of the expression used to compute the area of a circle,
the @fn[square] function will be invoked. This is visible in the following evaluation sequence:

@evaluation-steps[
(circle-area 2)
(* pi (square 2))
(* 3.14159 (square 2))
(* 3.14159 (* 2 2))
(* 3.14159 4)
12.5664
]

Since defining functions allows for associations to be established between a procedure
and a name, that means Racket needs to have memory in which to store those associations.
This memory is called @emph{environment}.

Note that this environment exists only while we are working. When the program is shut down,
that environment is lost. In order to avoid losing those definitions, they should be saved as
in a file. This is the reason why the process of working in Racket is based on writing the definitions
in files although still needing to test them using the evaluator to ensure the proper behaviour
of the created definitions.

@questions[
@question{Define the function @fn[double], that calculates the double of a given number.}
]

@section{Names}

The definition of functions in Racket involves assigning names: names for functions and names
for its parameters.

Racket presents almost no limit towards names that you can give. A name like @fn[square] is as valid as
@fn[x+y+z] because what separates a name from the other elements of a combination are parentheses and blank
spaces. @margin-note{The blank space concept includes tabs and changing line.}

In Racket the only characters that cannot be used in names are the parentheses @tt{(}, apostrophe
@tt{'}, quotation marks @tt{"} and semicolons @tt{;}. All other characters can be used in names,
but, in practice, the creation of names should take some rules in consideration:

@itemlist[
@item{You should only use letters of the alphabet, digits, arithmetic symbols and some punctuation characters like
exclamation and interrogation marks. For portability reasons, accented characters should be avoided.}
@item{If the function's name has more than one word, those words should be separated by an hyphen @lisp[-].
For example, a function that computes the are of a circle can have the name @fn[circle-area]. However, @fn[circlearea],
@verb{circle_area} and @fn[circle+area] are less appropriate.}
@item{If the name is associated with a question it should end with an interrogation mark - @lisp[?]. For
example, a function that tests if a number is even could be called @fn[even?].}
@item{If the function makes a conversion between two types of values, the name could include the name of both
types and an arrow between them, indicating the direction of the conversion. For example, a function that converts
euros into pounds could be called @fn[euros->pound] or better yet @fn[pound<-euro].}]

The choice of names will have a significant impact on the program's legibility. Let us consider for example
the area @${A} of a triangle with a base @${b} and a height @${c} which can be defined mathematically by:

@$${A(b,c) = \frac{b \cdot c}{2}} 

In Racket's language we will have:

@def[
(define (A b c) (/ (* b c) 2))
]

Note that the Racket definition is identical to the corresponding mathematical expression,
apart from the prefix notation and the @${=} symbol being @lisp[define]. However, if we did
not know beforehand what the function does, we will hardly understand it. Therefore, and contrary
to Mathematics, the names that we assign in Racket should have a clear meaning. Instead of writing
@lisp{A} it is preferable that we write @lisp{triangle-area} and instead of writing @lisp{b} and
@lisp{c} we should write @lisp{base} and @lisp{height} respectively. Taking these aspects in
consideration we can present a more meaningful definition:


@def[
(define (triangle-area base height) 
  (/ (* base height) 2))
]

As the number of definitions grow, names become particularly important for the reader to quickly
understand the written program, so it is crucial that names are carefully chosen.

@questions[
@question{Suggest an appropriate name for the following functions:
@itemlist[@item{Function that calculates the volume of a sphere;}
          @item{Function that tests if a number is a prime-number;}
          @item{Function that converts a measurement in centimetres into inches.}]
}

@question{Define the function @fn[radians<-degrees] that receives an angle in degrees and computes the corresponding value in radians. Note that @${180} degrees are @${pi} radians.
}

@question{Define the function @fn[degrees<-radians] that receives an angle in radians and computes the corresponding value in degrees.
}

@question{Define a function that calculates the perimeter of a circle given the radius @${r}.
}

@question{Define a function that calculates the volume of a parallelepiped from its length, width and height.
}

@question{Define a function that calculates the volume of a cylinder a length and radius. The volume corresponds to multiplying the base radius with the cylinder's length.
}

@question{Define a function @fn[average] that calculates the average value
between two numbers. For example: @lisp[(average 2 3)] @${\rightarrow}
@lisp[2.5].  } ]

@section{Predefined Functions}

The possibility of defining new functions is fundamental for increasing the language's
flexibility and its ability to adapt to the problems we want to solve. The new functions,
however, must be defined in terms of others that were either defined by the user or, at most,
were already pre-defined in the language.

As we will see, Racket has a reasonable set of predefined functions that in most cases they
are enough for what we want to do, but we should not try to avoid defining new functions
whenever we deem it necessary.

In @tabref{tab:funcPreDef} we see a set of mathematical functions predefined in Racket.
Note that, due to syntax limitations (that are also present in other languages), it is common
that some Racket functions have a notation that is different when compared to the mathematical
definition. For example, the square root function, @${\sqrt{x}} is written as @lisp[(sqrt x)].
The name @lisp[sqrt] is a contraction of the words @emph{square root} and similar contractions
are used for several other functions. The @emph{absolute value} function is written as @${|x|}
and the exponentiation function @${x^y} as @lisp[(expt _x _y)]. @Tabref{tab:funcEquiv} shows
some equivalents between the invocations of Rackets functions and the correspondent Mathematics
invocations.

@table[#:tag "tab:funcPreDef" #:caption @elem{Some math predefined functions in Racket.}
@tabular[#:style 'boxed
	 #:sep @hspace[1]
         #:column-properties '(left left left)
         #:row-properties '(bottom-border ())
         (list (list @bold{Function}   @bold{Arguments} @bold{Result})
	       (list @fn[+] @elem{Multiple Numbers} @para{The sum of all arguments. With no arguments, zero.})
	       (list @fn[-] @elem{Multiple Numbers} @para{With only one argument, the symmetric value.  For more than one argument, the subtraction of the first argument by all the remaining arguments. With no arguments, zero.})
	       (list @fn[*] @elem{Multiple Numbers} @elem{The multiplication of all arguments. With no arguments, zero.})
	       (list @fn[/] @elem{Multiple Numbers} @elem{The dividision of the first argument by all the remaining arguments. With no arguments, zero.})
	       (list @fn[add1] @elem{One number} @elem{The sum of the argument with 1.})
	       (list @fn[sub1] @elem{One number} @elem{The subtraction of the argument with 1.})
	       (list @fn[abs] @elem{One number} @elem{The absolute value of the argument.})
	       (list @fn[sin] @elem{One number} @elem{The sine of the argument (in radians).})
	       (list @fn[cos] @elem{One number} @elem{The cosine of the argument (in radians).})
	       (list @fn[atan]@elem{One or two numbers} @elem{With only one argument, the inverse tangent of the argument (in radians). With two arguments,the arc tangent of the division between the first and the second. The argument's sign is used to determine the quadrant.})
	       (list @fn[sqr] @elem{One number} @elem{The square of the argument.})
	       (list @fn[sqrt] @elem{One number} @elem{The square root of the argument.})
	       (list @fn[exp] @elem{One number} @elem{The exponential value with @${e} base of the argument.})
	       (list @fn[expt] @elem{Two numbers} @elem{The first argument raised to the second one.})
	       (list @fn[log] @elem{One number} @elem{The logarithmic value of the argument.})
	       (list @fn[max] @elem{Multiple Numbers} @elem{The highest argument.})
	       (list @fn[min] @elem{Multiple Numbers} @elem{the lowest argument.})
	       (list @fn[remainder] @elem{Two numbers} @elem{With two arguments, the remainder of the division between the first and the second argument.})
	       (list @fn[floor] @elem{One number} @elem{The argument without the fractional part.}))]
]

@table[#:tag "tab:funcEquiv" #:caption @elem{Racket's predefined math functions.}
@tabular[#:style 'boxed
	 #:sep @hspace[1]
         #:column-properties '(left left)
         #:row-properties '(bottom-border ())
         (list (list @bold{Racket}   @bold{Mathematics})
               (list @lisp[(+ _x0 _x1 ... _xn)] @${x_0 + x_1 + \ldots + x_n})
               (list @lisp[(+ _x)] @${x})
               (list @lisp[(+)] @${0})
               (list @lisp[(- _x0 _x1 ... _xn)] @${x_0 - x_1 - \ldots - x_n})
               (list @lisp[(- _x)] @${-x})
               (list @lisp[(-)] @elem{Error})
               (list @lisp[(* _x0 _x1 ... _xn)] @${x_0 \times  x_1 \times  \ldots \times  x_n})
               (list @lisp[(* _x)] @${x})
               (list @lisp[(*)] @${1})
               (list @lisp[(/ _x0 _x1 ... _xn)] @${x_0 / x_1 / \ldots / x_n})
               (list @lisp[(/ _x)] @${x})
               (list @lisp[(/)] @elem{Error})
               (list @lisp[(add1 _x)] @${x+1})
               (list @lisp[(sub1 _x)] @${x-1})
               (list @lisp[(abs _x)] @${|x|})
               (list @lisp[(sin _x)] @${\sin x})
               (list @lisp[(cos _x)] @${\cos x})
               (list @lisp[(atan _x)]@${\arctan x})
               (list @lisp[(atan _y _x)]@${\arctan \frac{y}{x}})
               (list @lisp[(sqr _x)] @${x^2})
               (list @lisp[(sqrt _x)] @${\sqrt{x}})
               (list @lisp[(exp _x)] @${e^x})
               (list @lisp[(expt _x _y)]@${x^y})
               (list @lisp[(log _x)] @${\log x})
               (list @lisp[(floor _x)] @${\lfloor x\rfloor}))]
]

@questions[
@question{Translate the following mathematical expressions into Racket's notation:
@itemlist[#:style 'ordered
                  @item{@${\sqrt{\frac{1}{\log 2^{\left|(3-9\log 25)\right|}}}}}
                  @item{@${\frac{\cos^4 \frac{2}{\sqrt 5}}{\arctan 3}}}
                  @item{@${\frac{1}{2} + \sqrt 3 + \sin^{\frac{5}{2}} 2}}]
}

@question{Translate the following Racket expressions into mathematical notation:
@itemlist[#:style 'ordered
                  @item{@lisp[(log (sin (+ (expt 2 4) (/ (floor (atan pi)) (sqrt 5)))))]}
                  @item{@lisp[(expt (cos (cos (cos 0.5))) 5)]}
                  @item{@lisp[(sin (/ (cos (/ (sin (/ pi 3)) 3)) 3))]}]
}

@question{Define the function @lisp[odd?] that, for a given number evaluates if it is odd,
i.e., if the remainder of that number when divided by two is one. In order to do so, you
can use the predefined function @lisp[remainder]}

@question{The area @${A} of a pentagon inscribed in a circle of radius @${r} is given by the following expression:
@$${A = \frac{5}{8}r^2\sqrt{10 + 2\sqrt{5}}}
Define a function to calculate that same area.}

@question{Define a function to calculate the volume of an ellipsoid
with semi-axis @${a}, @${b} and @${c}. That volume can be calculated using the formula:
@${V=\frac{4}{3}\pi a b c} } ]

@section{Arithmetic in Racket}

We saw previously that Racket is capable of dealing with several types of numbers, from integers to complex
numbers and also fractions. Some of those numbers like @${\sqrt 2} or @${pi}, do not have a rigorous
representation based in numerals and for that reason, Racket classifies them as inexact numbers, in
order to emphasize that they are dealing with an approximated value. When an inexact number is used in an
arithmetic operation, the result will also be inexact, so the inexactness is said to be @emph{contagious}.

@emph{Finitude} is another feature of the inexact numbers. Unlike exact numbers that theoretically
have no limits, inexact numbers cannot surpass a certain limit, above which every number is represented
by infinity, as you can see by the following example:

@incremental[
(expt 10.0 10)
(expt 10.0 100)
(expt 10.0 1000)
]

Note that @verb{|+inf.0|} (or @verb{|-inf.0|}) is Racket's way of saying that a number exceeds the
inexact numbers representation capacity. The number is not infinite, as one might think, but merely a
value excessively big for Racket's capacity.

There is also another problem regarding inexact numbers: round-off errors. As an example, let us
consider the obvious equality @${(4/3-1)*3-1=0} and let us compare the results that we get by using
exact or inexact numbers in Racket:

@incremental[
(- (* (- (/ 4 3) 1) 3) 1)
(- (* (- (/ 4.0 3.0) 1.0) 3.0) 1.0)
]

As we can see, by using inexact numbers we cannot obtain a correct result and the problem
is caused by the use of round-off errors: 3/4 can't be represented with a finite number of digits. This round-off
error is then propagated to the remaining operations which will produce a value that, even though not zero, is
relatively close enough. 

@questions[
@question{Translate the following definition into Racket:
@$${f(x)=x-0.1\cdot{}(10\cdot{}x-10)}
}

@question{In Mathematical terms, whatever the argument used in the previous function, the
result should always be @${1} because
@$${f(x)=x-0.1\cdot{}(10\cdot{}x-10)=x-(x-1)=1} Using the created
function calculate the result of the following expressions and explain them:

@lispcode[
(f 5.1)
(f 51367.7)
(f 176498634.7)
(f 1209983553611.9)
(f 19843566622234755.9)
(f 553774558711019983333.9)
]
}

@question{We wish to create a flight of stairs with @${n} treads, capable of covering a height @${a} in meters.
Considering that each step as a tread height @${h} and a width @${d} that obey to the following proportion:
@$${2h+d=0.64} 

define a function that, from a given height to cover and the number of treads, computes the length of the flight
of stairs.} ]

@section{Name Evaluation}

The primitive elements presented so far, such as numbers and strings, evaluate to
themselves, i.e., the value of an expression composed only by a primitive element
is the primitive element itself. With names this is no longer true.

Names have a special meaning in Racket. Note that when we define a
function, it has a name. And its formal parameters have names as
well. When a combination is written, the evaluator uses the function
definition associated with the name that is the first element of the
combination.  This means that the value of the first element in a
combination is the associated function.  If we had defined the
function @lisp[square], as suggested previously in section
@ref{sec:syntaxDefine}, we could test this behaviour in the following
expressions:

@incremental[
(square 3)
square
]

As we can see, the value of the name @lisp[square] is an entity that Racket
describes using a special notation. This entity is, as shown, a function. The
same behaviour happens to any other predefined function:

@incremental[+ *]

As we have seen, the sum @fn[+] and multiplication @fn[*] signs are some of the
predefined names of the language. For example, the symbol @lisp[pi] is also predefined and
associated with an approximated value of @${pi}:

@incremental[pi]

However, when the body of the expression is evaluated, the value of a name
assigned to a parameter in a function is the corresponding argument during the
function call. For example, in the combination @lisp[(square 3)], after the
evaluator knows that the @fn[square] value is a function by us defined and
that @lisp[3] has the value @${3}, it evaluates the body of the function but
associating the name @lisp[x], whenever is necessary, to the same @${3} that
was previously associated with @lisp[x].

@section{Conditional Expressions}

There are many operations in which the result depends on a test. For
example, the mathematical function @${|x|}, that estimates the absolute
value of @${x} is equivalent to the inverse of a number if it is negative
or the number itself otherwise. Using the mathematical notation we have that:

@$${|x|= 
\begin{cases}
-x, & \text{if $x<0$}\\
x, & \text{otherwise.}
\end{cases}}

This function needs to @emph{test} if the argument is negative and
choose one of two alternatives: it either evaluates for the number itself
or for its symmetrical value. 

These kind of expressions that depend on making one or more tests,
are called @emph{conditional expressions}.

@subsection{Logical Expressions}

A conditional expression follows the structure ''if @emph{expression}
then @ldots, otherwise @ldots''. The @emph{expression} that determines
whether to use the branch ''if'' or the branch ''otherwise'', is called
the @emph{logical expression} and is characterized for having its value
interpreted as either @emph{true} or @emph{false}. For example, the
logical expression @lisp[(< x 0)] tests if the value of @lisp[x] is less than
zero. If it is, the expression's evaluation will return true, otherwise it
will return false.

@subsection{Logical Values}

Some programming languages consider true and false as part of a special data
type called @emph{logical} or @emph{boolean} data.  @margin-note{After George
Boole, the English mathematician that invented the algebra of logic.} Other
languages, like Racket, do not consider that these values should be treated as
special data, just that the conditional expression considers some of the values
as true and the remaining ones as false.

In Racket, conditional expressions consider only one of its values as false,
represented as @lit[#f]. Any other value that is different than @lit[#f] is
considered to be true. From the conditional expression point of view, the expression
@${123} is considered to be true. However, it makes little sense to a human user that
a number is considered as true or false so a constant that represents true
was introduced the language. This constant is represented by @lit[#t]. If @lit[#f]
is the only value that represents falsehood and if @lit[#t] is different than @lit[#f]
then @lit[#t] necessarily represents truth.

@section{Predicates}

In the most usual case, a logical expression is a function applied to some arguments. 
In this case, the function used as the test case is known as a @emph{predicate}. The
test value is interpreted as true or false. So the predicate is a function that produces
only true or false.

Despite the use of @lit[#t] and @lit[#f] it is important to know that not every predicate
returns @lit[#t] and @lit[#f] exclusively. There are predicates produce different values
from @lit[#t] and @lit[#f].

@subsection{Arithmetic Predicates}

The mathematical @emph{relational operators} @${<},@${>},@${=},@${\leq} and @${\geq}
are some of the most simple predicates. These operators compare numbers between each other.
Their use in Racket follows the prefix notation and are written respectively @lisp[<],@lisp[>],@lisp[=],
@lisp[<=] and @lisp[>=]. Some examples are:

@incremental[
(> 4 3)
(< 4 3)
(<= (+ 2 3) (- 6 1))
]

@section{Logical Operators}

In order to combine logical expressions together we have the
@stx[and], @stx[or] and @fn[not] operators. The @stx[and] and the
@stx[or] operators accept any number of arguments. The @fn[not] only
accepts one. The value of such combinations is determined according to
the following rules:

@itemlist[
          @item{The @stx[and] evaluates arguments from left to right until one of them is false, returning that value. If none of the arguments are false, the @stx[and] operator returns true.}
          @item{The @stx[or] evaluates arguments from left to right until one of them is true, returning that value. If none of the arguments are true, the @stx[or] operator returns false.}
          @item{The @fn[not] operator evaluates for true, if the argument is false, and evaluates false if the argument is true.}]

Note that although the meaning of false is clear, it necessarily
corresponds to the value of @lit[#f], the meaning of true is not so clear
because everything different than @lit[#f] is considered to be true.

@questions[
@question{What is the value of the following expressions?
@itemlist[#:style 'ordered
          @item{@lisp[(and (or (> 2 3) (not (= 2 3))) (< 2 3))]}
          @item{@lisp[(not (or (= 1 2) (= 2 3)))]}
          @item{@lisp[(or (< 1 2) (= 1 2) (> 1 2))]}
          @item{@lisp[(and 1 2 3)]}
          @item{@lisp[(or 1 2 3)]}
          @item{@lisp[(and #f 2 3)]}
          @item{@lisp[(or #f #f 3)]}]
}
]

@section{Predicates with a Variable Number of Arguments}

An important property of the arithmetic predicates @fn[<],@fn[>],@fn[=],
@fn[<=] and @fn[>=] is they accept any number of arguments. Whenever there
is more than one argument, the predicate is applied sequentially to
pairs of argument. That way, @lispcode[(< #,(lispemphi e "1") #,(lispemphi e "2") #,(lispemphi e "3") ... #,(lispemphi e "n-1") #,(lispemphi e "n"))]
is equivalent to @lispcode[(and (< #,(lispemphi e "1") #,(lispemphi e "2")) 
                (< #,(lispemphi e "2") #,(lispemphi e "3")) 
                ...
                (< #,(lispemphi e "n-1") #,(lispemphi e "n")))]. This property can be seen in the following examples:


@incremental[
(< 1 2 3)
(< 1 2 2)
]

@section[#:tag "sec:reconhecedores"]{Recognizers}

Apart from relational operators, there are many other predicates in Racket, like @fn[zero?],
that tests if the number is zero:

@incremental[
(zero? 1)
(zero? 0)
]

Note that @fn[zero?] ends with an question mark because when a predicate is called,
a question is being asked. For historical reasons, not all predicates in Racket
follow this convention. However, when we define new predicates we should be mindful
to use them.

Note that the operator @fn[zero?] is used to recognize a particular element (zero) in
a data type (numbers). These type of predicates are known as @emph{recognizers}. 

Another important set of predicates are the @emph{universal recognizers}. These do not
recognize one but all elements of a particular type of data. An universal
recognizer accepts any kind of data as an argument and returns true if that elements
belongs to the that same kind.

For example, to determine if a certain element is a number, we can use the @fn[number?]
predicate:

@incremental[
(number? 1)
(number? "Two")
]

And the @fn[string?] predicate determines if entities are strings:

@incremental[
(string? "Two")
(string? 3)
]

Finally, there are predicates that recognize certain sub types of
data, like the predicate @fn[integer?] that recognizes integer values:

@incremental[
(integer? 1)
(integer? 1.0)
(integer? 1.1)
(integer? 2/3)
(integer? "four")
]


Note that similar to the number @lisp[1], the number @lisp[1.0] is also
an integer. The first is an exact number and the second one is an
inexact number which implies that operations involving @lisp[1.0] will
produce inexact results. To distinguish between both types of numbers
we have the predicates @fn[exact?] and the @fn[inexact?]:

@incremental[
(exact? 1)
(exact? 1.0)
(inexact? 1)
(exact? 2/3)
]


@questions[
@question{What is a conditional expression? What is a logical expression?
}
@question{What is a logical value? Which logic values does Racket incorporate?
}
@question{What is a predicate? Give examples of predicates used in Racket.
}
@question{What is a relational operator? Give examples of relational operators used in Racket.
}
@question{What is a logical operator? Give examples of logical operators used in Racket.
}
@question{What is a recognizer? What is an universal recognizer? Give examples in Racket.
}

@question{Translate the following mathematical expressions into Racket's notation: 
@itemlist[#:style 'ordered
 @item{@${x<y}}
 @item{@${x\leq y}}
 @item{@${x<y\wedge y<z}}
 @item{@${x<y\wedge x<z}}
 @item{@${x\leq y \leq z}}
 @item{@${x\leq y < z}}
 @item{@${x< y \leq z}}]
}
]

@section{Selection}

If we look at the mathematical definition of absolute value:

@$${
|x|= 
\begin{cases}
-x, & \text{if $x<0$}\\
x, & \text{otherwise}
\end{cases}} 

we notice that it uses a conditional expression in the form of

@$${\begin{cases}
\text{consequent expression}, & \text{if logical expression}\\
\text{alternative expression}, & \text{otherwise}
\end{cases}}

that translates to common language as “if @emph{logical expression} then
@emph{consequent expression}, otherwise @emph{alternative expression}”.

The evaluation of a conditional expression is made through the evaluation of
the @emph{logical expression} and if it is true, the @emph{consequent expression}
is applied, if it is false then the @emph{alternative expression} is applied.

The use of conditional expressions in Racket is even easier than in
mathematics because it is based on a simple operator, the @lisp[if]
operator, called a selection operator since it allows to choice
between two alternatives. The syntax of the @lisp[if] operator is as follows:

@specform[(if logical-expression consequent-expression alternative-expression)]

The value of a conditional expression that uses the operator @lisp[if] is computed
in the following way:

@itemlist[#:style 'ordered
                  @item{The @lispemph[logical-expression] is evaluated;}
                  @item{If the previous evaluation is true, then the combination value is the @lispemph[consequent-expression];}
                  @item{Otherwise, if the logical expression turns out to be false, the combination value is the @lispemph[alternative-expression].}]

Such behaviour, identical to what we would have in Mathematics, can be verified by the following examples: 

@incremental[
(if (> 3 2)
     1
     2)
(if (> 3 4)
     1
     2)]

Using the @lisp[if] operator, we can now define the absolute value function by
doing a simple translation from the mathematical definition to Racket:

@def[
(define (abs x)
  (if (< x 0)
    (- x)
    x))
]

The purpose of using the @lisp[if] operator is to define functions
whose behaviour depends on one or more conditions. For example, if we
consider the @fn[max] function that receives two numbers as arguments
and returns the highest. To define such function we only need to test
if the first number is higher than the second one. If it is, the
function returns the first argument, otherwise it returns the second
one. Based on this logic we can write:

@def[
(define (max x y)
  (if (> x y)
    x
    y))
]

Another far more interesting example is the mathematical function @emph{sign} @${\operatorname{sgn}},
also known as @emph{signum} (latim for "sign"). This function could be interpreted as the @emph{dual}
function of the absolute value function because we will have that @${x=\operatorname{sgn}(x) |x|}.
The sign function is defined as:

@$${\operatorname{sgn} x = \begin{cases} 
-1 & \text{if $x<0$} \\
 0 & \text{if $x = 0$} \\
 1 & \text{otherwise}\end{cases}}

In common language, we would say that if @${x} is negative, the @${\operatorname{sgn} x} value is
@${-1}, otherwise, if @${x} is @${0}, the value is @${0}, otherwise
the value is @${1}. That shows that the above expression uses two conditional expressions
stacked in the following way:

@$${\operatorname{sgn} x = \begin{cases} 
-1 & \text{se $x<0$} \\
 \begin{cases}
 0 & \text{se $x = 0$} \\
 1 & \text{otherwise}\end{cases}
& \text{otherwise}\end{cases}}

To define this function in Racket, two @lisp[if]s must be used:

@def[
(define (signum x)
  (if (< x 0)
    -1
    (if (= x 0)
      0
      1)))
]


@section{Multiple Selection}

When a conditional expression requires more than one @lisp[if], it is
possible that the code becomes increasingly harder to read. In this case there is
an alternative called @stx[cond] which makes the function's
definition easier to read. The syntax of @stx[cond] is as follows:

@lispcode[
(cond (#,(lispemphi expr "0,0") #,(lispemphi expr "0,1") ... #,(lispemphi expr "0,n"))
      (#,(lispemphi expr "1,0") #,(lispemphi expr "1,1") ... #,(lispemphi expr "1,m"))
      ...
      (#,(lispemphi expr "k,0") #,(lispemphi expr "k,1") ... #,(lispemphi expr "k,p")))]

A @stx[cond] receives any number of arguments. Each argument is
called a @emph{clause} and is made up of a list of expressions. The
semantics of a @stx[cond] is based on evaluating sequentially the
first expression in each clause until one of them turns out to be
true. In that case the consequent expressions are evaluated and the
value of the last one is returned. If none of the clauses has a first
expression that is true, the @stx[cond] returns @lisp[\#<void>] meaning
there is no relevant result to be returned. If the clause, in which the
first expression is true, has no other expressions then @stx[cond] returns
the value of that first expression.

It is important to note that the parenthesis around the clauses do not mean
the clauses are combinations: they are simply part of the @stx[cond] syntax
and are necessary to separate clauses from each other.

The usual pragmatic used in a @stx[cond] (especially when the clause only has
two expressions) consists in aligning the expressions one under the other. 

Using @stx[cond] the @emph{sign} function can be much easily defined:

@def[
(define (signum x)
  (cond ((< x 0)
         -1)
        ((= x 0)
         0)
        (#t
         1)))
]

Note that in the previous example the last @stx[cond] clause has, as
logical expression, the @lisp[#t] symbol. As we have seen, this last
symbol represents true so its presence ensures that it will be
evaluated in case none of the previous ones are. This way, a clause
in the form of @lisp[(#t ...)] represents a "for everything else....". 
To make the reading task easier, Racket accepts a special form for this
last situation: @stx[else]. Using that syntax, the example can now
be written as:

@def[
(define (signum x)
  (cond ((< x 0)
         -1)
        ((= x 0)
         0)
        (else
          1)))
]

@questions[
@question{Define the function @fn[sum-highest] that given 3 numbers as arguments calculates the sum of the 2 with the highest value.
}
@question{Define the function @fn[max3] that given 3 numbers as arguments returns the highest value. 
}
@question{Define the function @fn[second-highest] that given 3 numbers as arguments and returns the second highest number: the number between the maximum and minimum value.
}
]

@section{Local Variables}

Let us consider the following triangle:

@fig{
  @tex{
\begin{tikzpicture}[scale=2]
\inputtikz{triangulo}
\end{tikzpicture}}}

and try to define a function in Racket to calculate the triangle's area
from the parameters @${a}, @${b} and @${c}.

One way of calculating the area of a triangle is to use the famous Heron's formula:
@margin-note{Heron of Alexandria was an important Greek mathematician and engineer of the
1st century A.D. to whom numerous discoveries and inventions were credited to, including
the steam engine and the syringe.}

@$${A=\sqrt{s\left(s-a\right)\left(s-b\right)\left(s-c\right)}}

in which @${s} is the triangle's semi-perimeter: 

@$${s=\frac{a+b+c}{2}}

When trying to use the Heron's formula to write the equivalent in Racket we come across a small
problem: the formula is (also) written in terms of the semi-perimeter @${s}, but @${s} is not
a parameter but rather a value that is derived from other parameters of the triangle.

One way of solving this problem is to replace @${s} with its meaning:

@$${A=\sqrt{\frac{a+b+c}{2}\left(\frac{a+b+c}{2}-a\right)\left(\frac{a+b+c}{2}-b\right)\left(\frac{a+b+c}{2}-c\right)}}

From this formula it is now possible to define the function in Racket:

@def[
(define (area-triangulo a b c)
  (sqrt (* (/ (+ a b c) 2)
	   (- (/ (+ a b c) 2) a)
	   (- (/ (+ a b c) 2) b)
	   (- (/ (+ a b c) 2) c))))
]

Unfortunately, this definition has two problems. The first one is the loss of
correspondence between the original formula and the function definition, making
it harder to recognize as the Heron’s formula. The second one is that the
function is repeatedly using @lisp[(/ (+ a b c) 2)] which is a waste of human
effort, because we had to write it four times, and a waste of computational
effort, because the expression needs to be calculated four times, even though
we know it always has the same value.

In order to solve this problem, Racket allows the use of @emph{local variables}.
A local variable only has meaning in the context of a function and is used to
calculate intermediate values such as the semi-perimeter @lisp[s]. Using a local
variable we can rewrite the @fn[triangle-area] function:

@def[
(define (triangle-area a b c)
  (define s (/ (+ a b c) 2))
  (sqrt (* s (- s a) (- s b) (- s c))))
]

The semantics used when definition local variables is the same as the definition
of regular ones, with the added subtlety that its @emph{context}, i.e. the part of
the program in which the defined name can be used, is confined to the function that
it contains.

When calling the function @fn[triangle-area], giving it the arguments for the
corresponding parameters @lisp[a], @lisp[b] and @lisp[c], it starts by introducing
an additional name - @lisp[s] - associated to the value that comes from the expression
@lisp[(/ (+ a b c) 2)] and, in the context of than new name, evaluates the remaining
expressions in the function's body. In practice it is as if the function was stating:
"Knowing that @${s=\frac{a+b+c}{2}}, let us calculate
@${\sqrt{s\left(s-a\right)\left(s-b\right)\left(s-c\right)}}."

There is another way of defining local variables that, although semantically similar
to the previous one, has the advantage of being usable in any part where an
expression is expected. We can do that by using the @lisp[let] form. The redefinition 
of the previous function using the @lisp[let] form is as follows:

@def[
(define (triangle-area a b c)
  (let ((s (/ (+ a b c) 2)))
    (sqrt (* s (- s a) (- s b) (- s c)))))
]

@fn[Let] uses the following syntax: 

@lispcode[
(let ((#,(lispemphi name "0") #,(lispemphi expr "0"))
      (#,(lispemphi name "1") #,(lispemphi expr "1"))
      ...
      (#,(lispemphi name "n") #,(lispemphi expr "n")))
  #,(lispemphi expr "n+1")
  #,(lispemphi expr "n+2")
  ...
  #,(lispemphi expr "n+m"))]

The semantic of the @lisp[let] form consists of associating each name (#,(lispemphi name "i")
to the corresponding expression @(lispemphi expr "i") and, in the context established by that
association, evaluate the @racket[let]'s @emph{body}, i.e., evaluate the expressions from
@(lispemphi expr "n+1") to @(lispemphi expr "n+m") and return the value of the last one.

One important characteristic of the @lisp[let] form is the fact that the
association between expressions and names does not depend on other
associated names. This implies that the context for evaluating the body
of @lisp[let] is established just once, and not incrementally. This can
be explained in the following example:

@incremental[
(let ((pi 3)
      (prev-pi pi))
  (+ pi prev-pi))
]

However, if we want the context to be established incrementally, thus allowing
expressions to associate names that can depend on previous established associations,
Racket provides the form @lisp[let*]:
form:

@incremental[
(let* ((pi 3)
       (prev-pi pi))
  (+ pi prev-pi))
]

The @lisp[Let*] form is formally equivalent to a cascade of @lisp[let] forms, as can
be seen in the following example:

@incremental[
(let ((pi 3))
  (let ((prev-pi pi))
    (+ pi prev-pi)))
]

@section[#:tag "sec:globalVariables"]{Global Variables}

Contrary to local names that have a limited context, a @emph{global name},
is a name that can be seen in any context of our programs. Its context is
therefore the entire program. The name @lisp[pi] represents the constant
@${\pi=3.14159@ldots} and can be used in any part of our program. For that
reason, @lisp[pi] is a global name.

The definition of global names is the same to that of local names, with the
difference of being defined outside a function. Therefore, if we wish to
introduce a new global name, for example, for the @emph{golden ratio}:
@margin-note{Also known as @emph{gold proportion} and @emph{divine proportion}
among other names, and abbreviated to @${\phi} in honour of Fineas, a Greek
sculptor responsible for building the Parthenon where, supposedly, this golden
proportion was used. The golden ration was first introduced by Euclid when solving
the problem of dividing a line segment into two parts such that the ratio between
the line segment and the longest part was equal to the ratio between the longest part
and the shortest part. If @${a} is the length of the longest part and @${b} the
shortest part, the Euclid's problem is equivalent to @${\frac{a+b}{a}=\frac{a}{b}}.
As a result, @${a^2-ab-b^2=0} or @${a=\frac{b\pm\sqrt{b^2+4b^2}}{2}=b\frac{1\pm\sqrt{5}}{2}}.
What makes sense then is: @${a=b\frac{1+\sqrt{5}}{2}}. The expression for
calculating the golden ratio is thus: @${\phi=\frac{a}{b}=\frac{1+\sqrt{5}}{2}.}}

@$${\phi=\frac{1+\sqrt{5}}{2}\approx 1.6180339887}

We simply need to write:

@def[
(define golden-ratio (/ (+ 1 (sqrt 5)) 2))
]

From this moment on, the @emph{golden-ration} can be referenced in any
part of our program.

It should be warned that global names should be limited, when possible, to defining
just constants, like @lisp[2pi]. Other useful examples may be @lisp[pi/2],
@lisp[4pi] and @lisp[pi/4], as well their symmetric values, that are defined in terms of:

@def[
(define 2pi (* 2 pi))

(define pi/2 (/ pi 2))

(define 4pi (* 4 pi))

(define pi/4 (/ pi 4))

(define -pi (- pi))

(define -2pi (- 2pi))

(define -pi/2 (- pi/2))

(define -4pi (- 4pi))

(define -pi/4 (- pi/4))
]

@section{Modules}

Every functionality of Racket is stored and organized @emph{modules}. Every module
is a unit containing a set of definitions. The @emph{Racket} language is nothing more 
than an aggregation of modules that provide often required functionalities. There
are many other functionalities in modules that we can only have access to if we specifically
ask for it.

Take for example the following program:

@verbatim{
#lang racket
(define (square x)
  (* x x))

(define (circle-area r)
  (* pi (square r)))}

The first line indicates the language in which the program is written,
in this case, Racket. The following lines are the definitions of our
program that, naturally, make use of Racket's functionalities. For this
program that includes the @lisp[define] function, the arithmetic operation @lisp[*]
and the @lisp[pi] value. 

If it was necessary to have additional functionalities we would have to
@emph{require} it, using the form @lisp[require]. For example, if we want
to visualize the @${\sin} function graph, we could use the @lisp[plot] module,
like so:

@verbatim{
#lang racket
(require plot)

(plot (function sin (- pi) pi))}
                                               
The @lisp[plot] module used above provides the means to visualize graphs,
namely the functions @lisp[plot] and @lisp[function], with the remaining names
being provided by @lisp[Racket]'s module.

The @lisp[require] form can also be used in other ways, the most useful
being to access Racket's central modules repository, called @lisp[planet]
(@verb{http://planet.racket-lang.org/}). For example, for accessing the
@lisp[rosetta] module, whose author is @lisp[aml] we should write:

@verbatim{
#lang racket
(require (planet aml/rosetta))}

@questions[
@question{Define a module called @lisp[hyperbolic] with the three following functions: hyperbolic sin (@fn[sinh]), hyperbolic cosin (@fn[cosh]) and hyperbolic tangent (@fn[tanh]), based on the mathematical definitions:
@$${\sinh x = \frac{e^x-e^{-x}}{2}}
@$${\cosh x = \frac{e^x+e^{-x}}{2}}
@$${\tanh x = \frac{e^x-e^{-x}}{e^x+e^{-x}}}
}

@question{In the same module, define the inverse hyperbolic functions: @fn[asinh], @fn[acosh], and @fn[atanh], whose mathematical definitions are: 

@$${\sinh^{-1} x=\ln(x+\sqrt{x^2+1})}
@$${\cosh^{-1} x=\pm\ln(x+\sqrt{x^2-1})}
@$${\tanh^{-1} x=\begin{cases}
\frac{1}{2}\ln(\frac{1+x}{1-x}), & \text{se $|x|<1$}\\
\frac{1}{2}\ln(\frac{x+1}{x-1}), & \text{se $|x|>1$}
\end{cases}}
}

@question{Set the defined names, from the previous exercises, as available for other modules. Hint: See Racket's documentation on @lisp[provide].}
]