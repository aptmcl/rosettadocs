#lang scribble/manual
@(require "../rosetta.rkt")
@setup-math

@title{Recursion}

@section{Introduction}

We have seen previously that our functions, in order to do something
useful, must call other functions. For example, if we
already have the function that computes the square of a number and if
we want to define the function that computes the cube of a number, we
can easily do it by using the square and an additional multiplication, 
i.e.:

@lispcode[
(define (cube x)
  (* (square x) x))
]

Similarly, we can define the function @fn[fourth-power] in terms of
the @fn[cube] function and an additional multiplication:

@lispcode[
(define fourth-power (x)
  (* (cube x) x))
]

Obviously, we can continue to define new functions to compute larger
powers but this is a lengthy process and, moreover, it will always be
limited. It would be much more useful if we were able to generalize
this process and simply define the exponentiation function which, from
two numbers (@emph{base} and @emph{exponent}) computes the first one
raised to the power of the second one.

However, what we did for the @fn[fourth-power], the @fn[cube], and the
@fn[square] gives us an important clue: @emph{if we have a function that
computes the exponentiation with the immediately lower exponent, then we
only need one additional multiplication to compute the exponentiation with
the next exponent}.

In other words, we have:

@lispcode[
(define (power x n)
  (* (inferior-power x n) x))
]

Although we were able to simplify our power calculation problem, there
is still one unanswered question: how can we calculate the power
immediately below? The answer may not be obvious, but once understood it
is trivial: @emph{the exponentiation directly below to the
power of exponent n is the exponentiation to the power of @${n-1}}.
That implies that @lisp[(inferior-power x n)] is exactly the same as
@lisp[(power x (- n 1))]. Based on this idea, we can rewrite the previous definition:

@lispcode[
(define (power x n)
  (* (power x (- n 1)) x))
]

In spite of our ingenious solution this definition has a problem: regardless of
the exponentiation we try to compute, we will never be able to get the a final
result. In order to understand this problem, it is simpler to consider a real case:
let us try to calculate the third power of the number @${4}, i.e., @lisp[(power 4 3)].

To do this, according to the @fn[power] function definition, we will need to evaluate
the following expression:

@evaluation-steps[
(power 4 3)
(* (power 4 2) 4)
(* (* (power 4 1) 4) 4)
(* (* (* (power 4 0) 4) 4) 4)
(* (* (* (* (power 4 -1) 4) 4) 4) 4)
(* (* (* (* (* (power 4 -2) 4) 4) 4) 4) 4)
(* (* (* (* (* (* (power 4 -3) 4) 4) 4) 4) 4) 4)
]

It is obvious that this process will never finish. The problem is due
to the fact that we reduced the power calculation of a number
raised to an exponent to the power calculation of this number raised
to an exponent immediately below it, but we have not said in which
situation we already have a simple enough exponent to which the solution
is immediate. In which situation does this happen? We have seen that
when the exponent is @${2}, the @fn[square] function returns the correct answer,
so the case @${n = 2} is sufficiently simple. However, it is possible to have
an even simpler case: when the exponent is @${1}, the result is simply the base
value. Finally, the simplest case of all: when the exponent is @${0}, the result
is @${1}, regardless of the base value.  This last case is easy to understand
when we see that the evaluation of @lisp[(power 4 2)] (i.e., the forth power of
four) is reduced to @lisp[(* (* (power 4 0) 4)4)]. For this expression to be
similar to @lisp[(* 4 4)] it is necessary that the evalauation of @lisp[(power 4 0)]
produces @lisp[1].

We are now capable of defining the @fn[power] function correctly:

@itemlist[
@item{When the exponent is @${0} the result is @${1};}
@item{Otherwise, we calculate the power with the exponent immediately before and we multiply it to the base.}
]

@lispcode[
(define (power x n)
  (if (zero? n)
    1
    (* (power x (- n 1)) x)))
]

The previous example is an example of a @emph{recursive} function, i.e., it is a
function which is defined in terms of itself. In other words, a recursive function
is a function that calls itself inside its own definition. This
use is obvious when we "unwrap" the evaluation process for the @lisp[(power 4 3)]:

@evaluation-steps[
(power 4 3)
(* (power 4 2) 4)
(* (* (power 4 1) 4) 4)
(* (* (* (power 4 0) 4) 4) 4)
(* (* (* 1 4) 4) 4)
(* (* 4 4) 4)
(* 16 4)
64
]

The @emph{recursion} is the mechanism that allows a function to call
upon itself during its own evaluation process. Recursion is one of the most
important programming tools, so it is important we understand it well. Many
problems that seem to be complex, usually have surprisingly simple recursive solutions.

There are countless examples of recursive functions. One of the
simplest is the factorial function that is defined mathematically as:

@$${n!= 
\begin{cases}
1, & \text{if $n=0$}\\
n \cdot (n-1)!, & \text{otherwise}
\end{cases}}

The translation of this formula into Racket is straightforward:

@lispcode[
(define (factorial n)
  (if (zero? n)
     1
     (* n (factorial (- n 1)))))
]

It is important to notice that for all recursive functions there is:

@itemlist[
@item{The basic case (also called @emph{stopping criterion}) to which the result is immediately known.}
@item{A non basic case (also called @emph{recursive case}) in which an original problem is decomposed into a simpler sub-problem.}]

If we analyse the factorial function, the stopping criterion is the equality test
to zero @lisp[(zero? n)], the immediate result is @lisp[1], and the recursive case
is obviously @lisp[(* n (factorial (- n 1)))].

Usually, a recursive function is only correct if it has a conditional statement,
which identifies the basic case. However, this is not always the case. Invoking a recursive
function consists of successively solving simpler sub-problems until the simplest
case of all is reached, for which the result is immediate. This way, the most common
pattern to write a recursive function is:

@itemlist[
@item{Start by testing the basic cases.}
@item{Make a recursive call with a sub-problem increasingly closer to a basic case.}
@item{Use the result of the recursive call to produce the result of the original call.}
]

Given this pattern, the most common errors associated with recursive functions are:

@itemlist[
@item{Not detecting the basic case.}
@item{Recursion not reducing the complexity of the initial problem, i.e., not moving on to a simpler problem.}
@item{Not properly using the result of the recursion to produce the originally intended result.}
]

Note that a recursive function that works perfectly with the cases for
which it was created, can be completely wrong for other cases. The
@fn[factorial] function is an example: when the argument is negative,
the problem's complexity increases and becomes further away from the basic
case:

@evaluation-steps[
(factorial -1)
(-1 * (factorial -2))
(-1 * (-2 * (factorial -3)))
(-1 * (-2 * (* -3 (factorial -4))))
(-1 * (-2 * (-3 * (* -4 (factorial -5)))))
(-1 * (-2 * (-3 * (* -4 (* -5 (factorial -6))))))
...
]

The most frequent error in a recursive function occurs when it never stops,
either because the basic case in not correctly detected, or because the
recursion does not decrease the problem's complexity. In this case, the
number of recursive calls grows indefinitely until the computer's memory
is exhausted. At this point, the program generates an error message. In
Racket's case this error is not entirely obvious because the evaluator
only interrupts the evaluation, showing no results. Here is an example:

@lispcode[
> (factorial 3)
6
> (factorial -1)
ERROR
]

It is very important to correctly understand the concept of recursion.
Although, at first, it may be difficult to embrace fully its
implications, recursion allows us to achieve simple solutions to
apparently very complex problems.

@questions[
@question{
The Ackermann function is set to non-negative numbers as follows:
@$${A(m, n) =
 \begin{cases}
 n+1 & \text{if $m = 0$} \\
 A(m-1, 1) & \text{if $m > 0$ and $n = 0$} \\
 A(m-1, A(m, n-1)) & \text{if $m > 0$ and $n > 0$}\end{cases}
}
Define the Ackermann function in Racket.
}

@question{
What is the value of:
@itemlist[
  @item{@lisp[(ackermann 0 8)]}
  @item{@lisp[(ackermann 1 8)]}
  @item{@lisp[(ackermann 2 8)]}
  @item{@lisp[(ackermann 3 8)]}
  @item{@lisp[(ackermann 4 8)]}
]
@;{
@itemlist[
  @item{@${9 = 8+1}}
  @item{@${10 = 8+2}}
  @item{@${19 = 2\times 8+3}}
  @item{@${2045 = 2^{(8+3)}-3}}
  @item{@${2^{2^{2^{2^{2^{2^{2^{2}}}}}}}-3}}
]
}
}]

@section{Recursion in Architecture}

As we will see, recursion is also a fundamental concept in
architecture. As an example, let us consider a ladder profile, as outlined
in @figref{fig:esquemaEscada}, and let us imagine that we intend
to define a function called @fn[ladder] that, given the point @${P},
the length @${c} of the thread, the @${e} height of each riser,
and finally the number of steps @${n}, creates the stairs with the
first riser starting at @${P}. Given these parameters, the
definition of the function should start as:

@lispcode[
(define (ladder p c e n)
  ...)
]

@figure[#:tag "fig:esquemaEscada"
        #:caption @elem{Profile of a ladder with @${n} steps, with the fist step starting at the point @${P}, each step containing a thread @${c} and a riser @${e}.}]{
@tex{\begin{tikzpicture}
    \inputtikz{esquemaEscada}
  \end{tikzpicture}}
}

To implement this function we have to be able to decompose the problem
in less complex sub problems, and this is where recursion provides a
great help: it allows us to decompose the drawing of a ladder with
@${n} steps into the drawing of a step followed by the drawing of a
ladder with @${n-1} steps, as presented in the diagram of
@figref{fig:esquemaEscadaRecursiva}.

@figure[#:tag "fig:esquemaEscadaRecursiva"
	#:caption @elem{Decomposition of the design of a ladder of @${n} steps
    into the design of a ladder of @${n-1} steps.
    steps.}]{
    @tex{\begin{tikzpicture}
          \inputtikz{esquemaEscadaRecursiva}
	  \end{tikzpicture}}}

This means the function will be something like:

@lispcode[
(define (ladder p c e n)
  ...
  (step p c e)
  (ladder (+xy p c e) c e (- n 1)))
]

To draw a step we can define the following function that creates the segments
for the thread and riser:

@lispcode[
(define (step p c e)
  (line p (+y p e) (+xy p c e)))
]

The problem now is that the @fn[ladder] function needs to stop creating
steps at some point. It is easy to see that, when successively reducing
the number of steps, we should stop when that number is zero. Thus,
when asked to draw a ladder with zero steps, the @fn[ladder] function no
longer needs to do anything. This means that the function should have the
following form:

@lispcode[
(define (ladder p c e n)
  (if (= n 0)
    ...
    (begin
      (step c e)
      (ladder (+xy p c e) c e (- n 1)))))
]

What is left to be decided is what does the function produces as output
when it reaches the stopping condition. Since, in this case, what interests
us is the side effect resulting from invoking the function, it is less
relevant what it produces as a result, so we will stipulate that it will
produce @lit[#t] to show that all worked out well:

@lispcode[
(define (ladder p c e n)
  (if (= n 0)
    #t
    (begin
      (step p c e)
      (ladder (+xy p c e) c e (- n 1)))))
]

To see a more interesting example of recursion in Architecture, let us
consider the Saqqara Pyramid, illustrated in @figref{fig:saqqara}, built
by the architect Imhotep in XXVII century b.C.. This step pyramid is
considered to be the first pyramid in Egypt and the oldest monumental
stone construction in the world, being composed of six progressively
smaller mastabas, stacked on top of each other. Another way of looking at
this step pyramid is a mastaba on top of which stands another
smaller step pyramid.

@figure[#:tag "fig:saqqara"
              #:caption @elem{The Saqqara steps Pyramid. Photography by Charles J. Sharp.}]{
  @authorizedPhoto{piramides/Saqqara_pyramid}
}

Formally, we can define a pyramid of @${n} steps as a mastaba on top of which
stands a pyramid of @${n-1} steps. To complete this definition it
must be said that when the last mastababa is created, the pyramid of
@${0} steps at the top is, actually, non-existent.

Thus, when looking at the illustration in @figref{fig:esquemaSaqqara}, if
we consider that the centre of the base of the pyramid is the @${p}
position and the mastabas are several pyramid frustums, we can write:

@figure[#:tag "fig:esquemaSaqqara"
	#:caption @elem{Schematic step pyramid.}]{
  @tex{\centering
  \begin{tikzpicture}[scale=0.5]
    \inputtikz{esquemaSaqqara}
  \end{tikzpicture}}}

@lispcode[
(define (step-pyramid p b t h d n)
  (if (= n 0)
    #t
    (begin
      (regular-pyramid-frustum 4 p b 0 h t)
      (step-pyramid (+z p h) (- b d) (- t d) h d (- n 1)))))
]

An approximate example of the pyramid of Saqqara would then be:

@lispcode[
(step-pyramid (xyz 0 0 0) 120 115 20 15 6)
]

@questions[
@question{
The above definition does not accurately reflect the geometry of the
step pyramid of Saqqara, since it has sloped surfaces between each
mastaba, as can be seen in the following diagram, where we compare the
sections of the pyramid we defined (left) to the actual pyramid
of Saqqara (right):

@centered[@tex{
\begin{tikzpicture}[scale=0.3]
\inputtikz{saqqara}
\end{tikzpicture}
}]

Define a more rigorous version of the @fn[step-pyramid] function
which receives, in addition to the above parameters, the height of
each slope. Experiment with different parameters in order to produce
a model similar to the following one:

@fig[@autoimage{saqqaraB}]}

@question{
The @emph{false} arch is the oldest form of arch, formed by
parallelepipeds arranged horizontally like steps, forming an opening
that narrows towards the top, ending with a horizontal beam, as shown
in the following diagram:

@centered[@tex{
\smaller
\begin{tikzpicture}[scale=1.7]
\inputtikz{arcoFalso}
\end{tikzpicture}
}]

Assuming that the parallelepipeds have a square section measuring @${l}, the
opening reduction is @${\Delta_e}, equal in every step, and that the
point @${p} is the arc's centre point, define the @fn[false-arc]
function that, with the parameters @${p}, @${c}, @${e}, @${\Delta_e}
and @${l}, creates a false arc.}

@question{
Define the @fn[balanced-circles] function that allows the creation of any of the following illustrations:

@def/no-show[
(define (balanced-circles p r f)
  (if (< r 0.1)
    #t
    (begin
      (circle p r)
      (balanced-circles (+y p (* (+ 1 f) r)) (* f r) f))))
]

@def/no-results[
(balanced-circles (xy 0 0) 1.2 0.3)
(balanced-circles (xy 3 0) 1.2 0.5)
(balanced-circles (xy 6 0) 0.9 0.6)
(balanced-circles (xy 9 0) 0.5 0.8)
]

@centered[@(show-tikz 2)]

@;centered[@tex{\autodrawing{equilibrioCirculos}}]

Note that the circles have radii that are in a geometrical
progression ratio of @${f}, with @${0 <f < 1}. That way, each circle
(except the first one) has a radius that is the product of @${f} by
the radius of the largest circle in which it stands. The smallest circle
has a radius that is greater or equal to @${1}. The function
should have as parameters the centre point and the radius of the
larger circle, and also the reduction factor @${f}.}

@question{
Consider the drawing of circles as shown in the following image:
@centered[
@tex{
\begin{tikzpicture}
\inputtikz{circulos}
\end{tikzpicture}
}
]

Define a @fn[radial-circles] function that given the coordinates of the
rotation centre @${p}, the number of circles @${n}, the @${r_0}
translation radius, the circle radius @${r_1}, the initial angle @${\phi}
and the angle increment @${\Delta\phi}, draws the circles as shown in
the previous figure.

Test your function with the following expressions:

@def/no-show[
(define (radial-circles p n r0 r1 phi dphi)
  (if (= n 0)
    #t
    (begin
      (circle (+pol p r0 phi) r1)
      (radial-circles p (- n 1) r0 r1 (+ phi dphi) dphi))))
]

@def/no-results[
(radial-circles (xy 0 0) 10 1.5 0.3 0 (/ pi  5))
(radial-circles (xy 4 0) 20 1.5 0.3 0 (/ pi 10))
(radial-circles (xy 8 0) 40 1.5 0.3 0 (/ pi 20))
]

whose evaluation should generate the following image:

@centered{@(show-tikz 1.5)}
@;centered[@tex{\autodrawing{circulosRadiais}}]
}

@question{
Consider the design of symbolic flowers composed of an inner circle
around which radial circles are arranged corresponding to the
petals. These circles should be tangent to each other and to the inner
circle, as shown in the following image:

@def/no-show[
(define (flower p r n)
  (define coef (sin (/ pi n)))
  (define r1 (/ (* r coef) (- 1 coef)))
  (define r0 (+ r r1))
  (circle p r)
  (radial-circles p n r0 r1 0 (/ 2pi n)))
]

@def/no-results[
(flower (xy 0 0) 1 10)
]

@;centered[@tex{\autodrawing[0.5]{flor}}]
@centered{@(show-tikz 1.5)}

Define a @fn[flower] function that receives only the flower's
centre point, the radius of the inner circle and the number of petals.

Test your function with the following expressions:

@def/no-results[
(flower (x 0.0) 1.0 10)
(flower (x 3.6) 0.4 10)
(flower (x 8.0) 2.0 20)
]

Their evaluation should generate the following image:

@centered{@(show-tikz 1.5)}
@;centered[@tex{\autodrawing{flores}}]
}
]

@section{Debugging Recursive Programs}

We saw in the @ref{sec:Depur} section that errors in a program can be
classified as syntactic or semantic errors. Syntactic errors occur
whenever we write invalid phrases in that language, i.e., phrases that
do not obey the grammar rules. Semantic errors are more complex
than the syntactic in that they generally can only be detected
during the program's execution. @footnote{There are some semantic
errors that can be detected @emph{before} the program's execution, but
this detection depends strongly on the quality of the language
implementation and its ability to anticipate the program's
consequences.} For example, if we try to calculate the factorial of a
@emph{string} we will have a semantic error, as shown in the
following example:

@lispcode[
> (factorial 5)
120
> (factorial "five")
zero?: contract violation
  expected: number?
  given: "five"
]

Obviously this last error has nothing to do with Racket's grammar
rules: the "sentence" on how to call the @fn[factorial] function is
correct. The problem is that it does not make sense to calculate the
factorial of a @emph{string}, because calculating factorials involves
arithmetic operations and these do not apply to @emph{strings}.
Therefore, the error is related to the meaning of the written "sentence",
i.e., with the semantics. It is then, a semantic error.

An infinite recursion is another example of a semantic error. We saw that
if the factorial function is called with a negative argument, infinite
recursion occurs. Consequently, if we use a negative argument, we will
be making a semantic error.

Racket provides several mechanisms for detecting errors. One of the
simplest ones is the @stx[trace] form (provided by the @lisp[racket/trace]
module) that allows the visualization of a function's invocations. The
@stx[trace] receives the name of the functions to be analysed and changes
these functions so as to write the successive calls with the respective arguments,
as well as the result of the its invocation. The information given as
the @emph{trace} result is generally extremely useful for debugging
functions.

For example, to visualize the call of the @fn[factorial] function,
consider the following definition:

@verbatim{
#lang racket
(require racket/trace)

(define (factorial n)
  (if (zero? n)
     1
     (* n (factorial (- n 1)))))

(trace factorial)
}

and the following call:

@verbatim{
> (factorial 5)
>(factorial 5)
> (factorial 4)
> >(factorial 3)
> > (factorial 2)
> > >(factorial 1)
> > > (factorial 0)
< < < 1
< < <1
< < 2
< <6
< 24
<120
120
}

Note that, in the previous example, as a consequence of @stx[trace],
the call of the @fn[factorial] function appears aligned to the left
side. Each recursive call appears slightly to the right, allowing the
display of the recursion's "depth", i.e., the number of recursive
calls. The result returned by each call appears aligned in the same
column of that call.

@questions[
@question{
@emph{Trace} the power function. What is the resulting @emph{trace} of the @lisp[(power 2 10)] evaluation?
}

@question{
Define a @fn[circles] function capable of creating the illustration presented below:

@def/no-show[
(define (circles p r)
  (if (< r 0.1)
    #t
    (begin
      (circle p r)
      (circles (+x p r) (/ r 2))
      (circles (+y p r) (/ r 2)))))

(circles (xy 0 0) 10)
]

@centered[@(show-tikz 0.4 "ultra thick")]

Note that the circles have radii that are in geometrical progression
ratio of @${\frac{1}{2}}. In other words, the smaller circles
have half the radius of the adjacent larger circle. The smallest
circles of all have a radius greater or equal to @${0.1}. The function should
only have as parameters the centre and radius of the larger circle.
}

@question{
Define the @fn[saw] function that, given a point @${P}, a number of
teeth, the length @${c} of each tooth, and the height @${a} of each
tooth, draws a saw, with the first tooth starting at @${P} as presented
in the following image:

@centered[@tex{
\begin{tikzpicture}
\inputtikz{serra}
\end{tikzpicture}
}]
}

@question{
Define the @fn[lozenges] function capable of creating the illustration presented below:

@def/no-show[
(define (lozenges p c min-c)
  (if (< c min-c)
    #t
    (let ((p0 (+pol p c 0))
          (p1 (+pol p c pi/2))
          (p2 (+pol p c pi))
          (p3 (+pol p c 3pi/2))
          (c2 (/ c 2.0)))
      (polygon p0 p1 p2 p3)
      (lozenges p0 c2 min-c)
      (lozenges p1 c2 min-c)
      (lozenges p2 c2 min-c)
      (lozenges p3 c2 min-c))))
]
@def/no-results[
(lozenges (xy 0 0) 1.6 0.1)
]

@centered{@(show-tikz 2)}
@;centered[@tex{\autodrawing[0.4]{losangos}}]

Note that the dimensions of the lozenges have a geometrical progression
ratio of @${\frac{1}{2}}. In other words, the smaller lozenges have half
the size of the largest lozenges at the tips of which they are centred.
The smallest lozenges have a width greater or equal to @${1}. This function
should only have as parameters the centre and width of the largest lozenge.
}

@question{
Consider the stair outlined in the figure below, designed to overcome an slope @${\alpha}.

@centered[@tex{
\begin{tikzpicture}[scale=0.5]
\inputtikz{escadaRampa}
\end{tikzpicture}    
}]

Define the @fn[stair-slope] function that receives the point @${p},
the angle @${\alpha}, the length @${c}, and the number of steps @${n},
and builds the ladder described in the previous schema.
}

@question{
Consider the ladder outlined in the figure below, designed to overcome a slope @${\alpha}.

@centered[
@tex{\begin{tikzpicture}[scale=0.5]
      \inputtikz{escadaProgressaoGeometrica}
    \end{tikzpicture}}]

Note that the steps' dimensions have a geometric progression ratio of
@${f}, i.e., given a step with a length of @${c}, the step immediately
above has a length of @${f \cdot c}. Define the @fn[geometric-progression-ladder]
function that receives the point @${P}, the angle @${\alpha}, the length @${c}, the
number of steps @${n}, and the ratio @${f}, and creates the ladder described
in the previous schema.
}
]

@section{Doric Temples}

With Vitruvius, we have seen that the Greeks created an elaborate
proportion system for columns. These columns were used to form
@emph{porticos}, wherein a succession of columns crowned with a roof served
as the entrance for buildings and, in particular, for temples. When this
arrangement of columns was placed in front of the building it was called prostyle,
and it was classified according to the number of columns in its composition
as Diastyle, Tristyle, Tetrastyle, Pentastyle, Hexastyle, etc. When the prostyle
was extended to the whole building, placing columns all around it, it was called
peristyle.

In addition to describing the proportions of columns, Vitruvius also explained in
his famous treaty the rules that the construction of temples had to follow, in particular,
regarding their orientation, which should be from east to west, and regarding the
spacing between columns, distinguishing several cases of temples from those with a
very reduced spacing (@emph{pycnostyle}), to temples with excessively large columns
spacing (@emph{araeostilo}), including the style which he considered to have the best
proportion (@emph{eustilo}), in which the spacing between columns is variable, being
larger in the central columns.

To simplify our implementation we will ignore these details and,instead of distinguishing
each @emph{style}, we will simply consider consider the placement of columns distributed
linearly in a particular direction, as sketched in @figref{fig:colunasLinhaRecta}.

@figure[#:tag "fig:colunasLinhaRecta"
	#:caption @elem{Temple's floor plan with an arbitrary orientation}]{
@tex{\centering
  \begin{tikzpicture}[scale=1.2]
    \inputtikz{colunasLinhaRecta}
  \end{tikzpicture}}
}

To this moment, we have considered coordinates as mere positions in space. Now, in
order to model this temple, it will be useful to adopt a @emph{vectorial}
perspective of the concept of coordinates. In this perspective, coordinates are seen
as the tips of vectors positioned at the origin. This can be seen in @figref{fig:colunasLinhaRecta},
where we have marked the position of two columns with the vectors @${P} and @${Q}.

With all vectors being positioned at the origin it becomes irrelevant to mention it, which
allows us to characterize vectors as having only a magnitude and a direction.  It is easy to
see that this magnitude and direction are precisely the polar coordinates of the vector's end,
i.e., the distance from the origin to the tip of the vector and the angle that the vector makes
with the @${X} axis.

The great advantage of adopting the vectorial perspective is that it enables us to conceive an
@emph{algebra} for operating with vectors. For example, the sum of vectors is a vector whose components
are the sum of corresponding components, i.e., 
@$${P + Q = (P_x + Q_x, P_y + Q_y, P_z + Q_z)}

Similarly, we can define the subtraction of vectors as @$${P - Q
= (P_x - Q_x, P_y - Q_y, P_z - Q_z)} and the multiplication of a vector with a scalar @${\alpha}:
@$${P\cdot\alpha=(P_x\cdot\alpha,P_y\cdot\alpha,P_z\cdot \alpha)}

Finally, the division of a vector by a scalar can be defined in terms of the
multiplication by the scalar's inverse value, i.e., 
@$${\frac{P}{\alpha} = P\frac{1}{\alpha}}

These operations are presented in @figref{fig:vectorespm} for the bi-dimensional case.

@figure[#:tag "fig:vectorespm"
	#:caption @elem{Algebraic operations using vectors.}]{
@tex{\centering
\begin{tikzpicture}[scale=0.8]
\inputtikz{vectorespm}
\end{tikzpicture}  
}}

Translating these definitions for Racket is straightforward. Since we are
creating algebraic operations for coordinates, we shall name them by 
combining the names of the arithmetic operators with the letter
"@lisp[c]" (of "@lisp[c]oordinates"). @footnote{These operations are
pre-defined in Racket.}

@lispcode[
(define (+c p0 p1)
  (xyz (+ (cx p0) (cx p1))
       (+ (cy p0) (cy p1))
       (+ (cz p0) (cz p1))))

(define (-c p0 p1)
  (xyz (- (cx p0) (cx p1))
       (- (cy p0) (cy p1))
       (- (cz p0) (cz p1))))

(define (*c p a)
  (xyz (* (cx p) a)
       (* (cy p) a)
       (* (cz p) a)))

(define (/c p a)
  (*c p (/ 1 a)))
]

@questions[
@question{
A @emph{unit} vector is a vector of unitary magnitude. Define the
@fn[unit-vector] operation that given any vector calculates the
unit vector with the same direction as the given one.}

@question{
The @emph{symmetric} vector of a @${\vec{v}} vector is the
vector @${\vec{v}^\prime}, such that @${\vec{v}+\vec{v}^\prime=0}. In
other words, is the vector with equal magnitude but opposite
direction. Define the @fn[symmetric-vector] operation that given
any vector calculates the symmetric vector.}
]

Returning to the problem of positioning columns in the temple, illustrated in
@figref{fig:colunasLinhaRecta}. Given the orientation and separation vector
@${\vec{v}} of columns, from the position of any column @${P}, we determine
the position of the next column through @${P+\vec{v}}. This reasoning allows
us to first define a function to create a row of columns. This function will
have as parameters the coordinates @${P} of the base of the first column, the
height @${h} of the column, the vector @${\vec{v}} separating the axes of the
columns, and also the number @${n} of columns that we wish to create. The reasoning
behind the definition of this function is, once more, recursive:

@itemlist[
@item{If the number of columns to create is zero, then there is nothing to
be done, so we can simply return @lit[#t].}
@item{Otherwise, we place a column at the point @${P}, and
recursively create the remaining columns based on the resulting point
of adding the vector @${v} to the point @${P}. As they are two
actions that we wish to perform sequentially, we need to use the operator
@stx{begin} to group a joint action.}
]

The translation of this process to Racket is the following:

@lispcode[
(define (doric-columns p h v n)
  (if (= n 0)
    #t
    (begin
      (doric-column p h)
      (doric-columns (+c p v) 
                     h
                     v
                     (- n 1)))))
]

We can test the creation of the columns using, for example:

@lispcode[
(doric-columns (xy 0 0) 10 (xy 5 0) 8)
]

from which we obtain the result presented in @figref{fig:colunasDoricas4}.

@figure[#:tag "fig:colunasDoricas4"
	#:caption @elem{A perspective view of a set of eight Doric columns with
   @${10} units of height and @${5} spacing units between columns along the @${x} axis.}]{
@tex{\centering
  \autoimage{colunasFila}
}}

@question{
Although the use of separation vectors between columns is relatively simple, it is
possible to simplify that process even further by calculating the vector value based
on the start and end points of the row of columns. Using the @fn[doric-columns] function,
define a function called @fn[doric-columns-between] that given the centre points @${P} and @${Q}
of the first and final columns, the height @${h} of the columns, and finally the number of
columns, creates a row of columns between those two points.

As an example, the following image shows the result of evaluating the following expressions:

@lispcode[
(doric-columns-between (pol 10 0.0) (pol 50 0.0) 8 6)
(doric-columns-between (pol 10 0.4) (pol 50 0.4) 8 6)
(doric-columns-between (pol 10 0.8) (pol 50 0.8) 8 6)
(doric-columns-between (pol 10 1.2) (pol 50 1.2) 8 6)
(doric-columns-between (pol 10 1.6) (pol 50 1.6) 8 6)
]

@fig[@autoimage{colunasLeque}]}

From the moment we know how to create rows of columns, it becomes easy
to create the four necessary rows for building peristyle temples.
Normally, the description of these temples is done in terms
of the number of columns at the front and at the side, assuming
that the columns at the corners count for both rows. This means that,
for example, in a temple with @${6 \times 12} columns, there are actually
only @${4 \times 2 + 10 \times 2 + 4 = 32} columns. To create the
peristyle, in addition to the number of columns at the front and side, we need to
know the position of the columns at the extremities of the temple and, obviously, the
column's height.

In terms of the algorithm, we start by creating one of the corners of
peristyle:

@lispcode[
(define (doric-peristyle-corner p height v0 n0 v1 n1)
  (doric-columns p height v0 n0)
  (doric-columns (+c p v1) height v1 (- n1 1)))
]

Note that, in order to avoid repeating columns, the second row must
start at the second column and consequentially one less column must be
placed.

To build the complete peristyle we only have to create one corner and
then build another corner with one less column on each side,
progressing in opposite directions.

@lispcode[
(define (doric-peristyle p height v0 n0 v1 n1)
  (doric-peristyle-corner p height v0 n0 v1 n1)
  (doric-peristyle-corner
    (+c p (+c (*c v0 (- n0 1)) (*c v1 (- n1 1))))
    height
    (*c v0 -1) (- n0 1) (*c v1 -1) (- n1 1)))
]

A realistic example is the temple of Segesta, represented in
@figref{fig:segesta}. This temple is of the peristyle type, composed of
@${6} columns (i.e., Hexastyle) at each front and @${14} columns at
the side, a total of @${36} @${9} meters high columns. The distance between
the columns axes is approximately @${4.8} meters at the front and
@${4.6} meters at the sides. The expression that creates the peristyle
of this temple is then:

@lispcode[
(doric-peristyle (xy 0 0) 9 (xy 4.8 0) 6 (xy 0 4.6) 14)
]

The result of evaluating the expression above is shown in @figref{fig:segesta2}.

@figure[#:tag "fig:segesta2"
	#:caption @elem{An overview of the peristyle of the temple of Segesta. The columns were 
                        generated by the @fn[peristyle-doric] function, using as parameters
                        @${6} columns on the front and @${14} on the side, with a column distance of @${4.8} meters 
                        on the front and @${4.6} meters on the sides, with columns @${9} meters high.}]{
@tex{\centering
  \autoimage{colunasPeristilo}}}

Although the vast majority of Greek temples were rectangular, circular temples
were also built, called @emph{Tholos}. The Sanctuary of Athena Pronaia at Delphi 
has a good example of one such buildings. Although little remains of this temple, it
is not difficult to imagine its original shape based on what still remains, shown in
@figref{fig:tholosAtena2}.

@figure[#:tag "fig:tholosAtena2"
	#:caption @elem{The Temple of Pronaia in Delphi, built in IV century b.C.. Photography by Michelle Kelley.}]{
@authorizedPhoto{tholos/MichelleKelley}}

To simplify the construction of the @emph{Tholos} temple, we will
divide it into two parts. In one of them we will create the base and
on the second one we will position the columns.

To design the base, we can consider a set of flattened cylinders,
stacked to form the circular steps, as shown in @figref{fig:esquemaBaseTholos}.
Thus, the total base height @${a_b} will be divided in steps of @${\Delta a_b}, and the base radius will
also be divided in @${\Delta r_b} steps.

@figure[#:tag "fig:esquemaBaseTholos"
	#:caption @elem{A @emph{Tholos} base section. The base is composed by a sequence of stacked cylinders, whose base
	radius @${R_B} shrinks @${\Delta r_b} each step and whose height grows in increments of @${\Delta a_b} in every step.}]{
@tex{\centering
\begin{tikzpicture}
\inputtikz{esquemaBaseTholos}
\end{tikzpicture}
}}

For each cylinder, we have to consider its @lisp[radius] and the
@lisp[d-height] of each step. To draw the next cylinder, we have also
to consider the radius increment @${d-radius}, due to the step's
length. These steps will be built using a recursive process:

@itemlist[
@item{If the number of steps to create is zero, nothing needs to be done.}
@item{Otherwise, we place a step (modelled with a cylinder) with the given radius and height and, recursively,
we place the remaining steps on top of this one, i.e., at a height equal to the steps being placed, and with a radius
reduced from the length of the steps being placed.}
]

This process is implemented by the following function:

@lispcode[
(define (base-tholos p n-steps radius d-height d-radius)
  (if (= n-steps 0)
    #t
    (begin
      (cylinder p radius d-height)
      (base-tholos (+xyz p 0 0 d-height)
                   (- n-steps 1)
                   (- radius d-radius)
                   d-height
                   d-radius))))
]

For the positioning of columns, we will also consider a process wherein
at each step we only place one column at a given position and, recursively,
we place the remaining columns from the next circular position.

Given its circular structure, the construction of this type of
building is simplified by the use of polar coordinates. In
fact, we can elaborate a recursive process that, from the
peristyle radius @${r_p} and the initial angle @${\phi}, places a column in
that position and then places the other columns using the same radius but
increments of @${\Delta\phi} to @${\phi}, as shown in
@figref{fig:esquemaTholos}. The angular increment @${\Delta\phi} is
obtained by dividing the circumference by the @${n} number of columns
to place, i.e., @${\Delta\phi=\frac{2\pi}{n}}. Since the columns are
arranged around a circle, the calculation of the coordinates of each
column is facilitated with the use of polar coordinates. With this algorithm
in mind,the function definition is as follows:

@figure[#:tag "fig:esquemaTholos"
	#:caption @elem{Schema of the construction of a @emph{Tholos}: @${r_b} is the base radius, @${r_p} is the
	distance from the centre of the columns to the centre of the base, @${a_p} is the column's height,
	@${a_b} is the base's height, the @${\phi} is the initial angle of the columns, and @${\Delta\phi} is
	the angle between columns.}]{
@tex{\centering
\begin{tikzpicture}[scale=5]
\inputtikz{esquemaTholos}
\end{tikzpicture}
}}

@lispcode[
(define (tholos-columns p n-columns radius fi d-fi height)
  (if (= n-columns 0)
    #t
    (begin
      (doric-column (+pol p radius fi) height)
      (tholos-columns p
                      (- n-columns 1)
                      radius
                      (+ fi d-fi)
                      d-fi
                      height))))
]

Finally, we define the function @fn[tholos], which, given the necessary parameters
to the two previous functions, invokes them sequentially:

@lispcode[
(define (tholos p n-steps rb dab drb n-colums rp ap)
  (base-tholos p n-steps rb dab drb)
  (tholos-columns (+xyz p 0 0 (* n-steps dab))
                  n-columns
                  rp
                  0
                  (/ 2pi n-columns)
                  ap))
]

@Figref{fig:tholosAthena} shows the image generated by the evaluation
of the following expression:

@lispcode[
(tholos (xyz 0 0 0) 3 7.9 0.2 0.2 20 7 4)
]

@figure[#:tag "fig:tholosAthena"
	#:caption @elem{Perspective of the @emph{Tholos} of Athens in Delphi, with @${20} @${4} meters high Doric columns,
	placed within a circle of @${7} meters radius.}]{
  @autoimage{tholos}
}

@question{A closer look to the @emph{Tholos} in
@figref{fig:tholosAthena} shows that there is an error: the abacus of
the various columns are parallel to each other (and also to the
abscissas and ordinate axes) when, in fact, they should have a radial
orientation. This difference is evident when we compare a top view
of the current design (left) with the view of the correct design
(right):

@fig[@autoimage{tholosRodado}]

Redefine the @fn[tholos-columns] function so that each column
is oriented properly in relation to the @emph{Tholos} centre.
}

@question{
Consider the construction of a tower made of several modules, in which
each module has exactly the same characteristics of a @emph{Tholos},
as presented in the figure below, on the left side:

@fig[@autoimage{torreTholos}]

The top of the tower has a similar shape to the @emph{Tholos} base but
with more steps.

Define the function @fn[Tholos-tower] that, from the centre of the
tower's base, the number of modules, the top number of steps, and other
required parameters to define a module identical to the @emph{Tholos},
builds the tower presented above.

Try to create a tower with @${6} modules, @${10} steps at the top,
@${3} steps per module, each with the same length and height @${0.2},
with @${7.9} base radius, and with @${20} columns per module, with a
peristyle radius of @${7} and with columns measuring @${4} meters. }

@question{
Based on the previous answer, redefine the tower's construction so
that the radial dimension decreases with the height, like the tower
on the centre of the previous image.  }


@question{
Based on the previous answer, redefine the tower's construction so
that the number of columns decreases with height, like the tower on
the right side of the previous image.  }

@question{
Consider the creation of a city in space, composed only by cylinders
of progressively smaller sizes, joined together by small spheres, as
shown in the perspective of the stereoscopic image: @footnote{In order
to see the stereoscopic image, focus your attention in the middle of
the two images and cross your eyes, as if you were to focus on a very
close object. You will notice that the two images become four,
although slightly blurry. Then try to uncross the eyes in order to see
only three images, i.e., until the two central images are
overlapped. Concentrate on that overlap and let your eyes relax until
the image is focused.}

@fig[@autoimage{cidadeEspacialStereo}]

Define a function that, starting from the city centre and the central
cylinders radius, creates a city similar to the presented one.}


@section{Ionic Order}

The @emph{volute} was one of the architectural elements introduced in
the transition from the Doric Order to the Ionic Order. A volute is a
spiral-shaped ornament placed at the top of a Ionian
capital. @Figref{fig:voluta} shows an example of an Ionian capital
containing two volutes. Although we still have numerous samples of
volutes from the antiquity, their drawing process was never clear.

@figure[#:tag "fig:voluta"
	#:caption @elem{Volutes of an Ionic capital. Photography by See Wah Cheng.}]{
@authorizedPhoto{espiral/SeeWah}}

Vitruvius, in his architectural treatise, describes the Ionian
volute: a spiral-shaped curve that starts at the base of the abacus,
unfolds into a series of turns and joins with a circular element, called the
@emph{eye}. Vitruvius describes the spiral drawing process through a
composition of fourths of a circumference, starting at the outermost point
and decreasing the radius in each quarter of a circumference, until joining with
the eye. In this description there are still some
details to be explained, in particular the position the centres of the fourths of
circumference. Vitruvius adds that a calculation and a figure will be added
at the end of the book.

Unfortunately such figure and calculation were never
found, leaving the drawing process of this fundamental element described by
Vitruvius still unclear. The doubts regarding that detail become even more
evident when analysis performed on the many volutes that survived the antiquity
revealed differences to the proportions described by Vitruvius.

During the Renaissance period, these doubts made researchers
rethink Vitruvius' method and suggest personal interpretations or
new methods for designing volutes. Of particular relevance are the methods
proposed in the sixteenth century by Sebastiano Serlio, based on the
composition of the semi-circumferences, by Giuseppe Salviati, based on the
composition of quarters of a circumferences, and by
Guillaume Philandrier, based on the composition of eighths of a
circumference.

All those methods differ in many details but, generically, they are all
based on using arcs of a circumference of constant angles but with a decreasing
radius. Obviously, for there to be continuity between the arcs, their centres
change as they are being drawn. @Figref{fig:spiral} presents
the process of drawing spirals using quarters of a circumference.

@figure[#:tag "fig:spiral"
	#:caption @elem{Design of a spiral with circular arcs.}]{
@tex{\centering
\begin{tikzpicture}[scale=5]
\inputtikz{esquemaEspiral}
\end{tikzpicture}
}}

As shown in this figure, in order to draw the spiral we must draw
successive circumference quarters. The first circumference quarter
will be centred at point @${p} with a radius @${r}. This first
radius goes from the angle @${\pi/2} to @${\pi}. The second
circumference quarter will be centred at point @${P_1} and will have
a radius of @${r\cdot f}, with @${f} being the "reduction" factor for the spiral.
This second arc goes from @${\pi} to @${\frac{3}{2}\pi}. An important detail
is the relationship between the coordinates of @${P} and @${P_1}: for the second
arc to have its end coincidental with the first arc, its centre must be at the end of the
vector @${\vec{v}_0} starting at @${P}, measuring @${r\cdot(1-f)} and with an
angle equal to the final angle of the end of the first arc.

This process should be repeated for the remaining arcs, i.e., we
will have to calculate the coordinates @${P_2}, @${P_3}, etc., as well
as the radii @${r\cdot f \cdot f}, @${r\cdot f \cdot f \cdot f}, etc.,
necessary to trace the successive circumference arcs.

When described in such way, the drawing process seems to be
complicated. However, it is possible to rewrite it so that it becomes
much simpler. In fact, is possible to think of the drawing of the overall spiral
as the drawing of a circumference quarter followed by the drawing of
a smaller spiral. More accurately, we can specify the drawing of a
spiral centred at the point @${P}, radius @${r} and
initial angle @${\alpha} as a drawing of a circumference arc of radius @${r},
centred at @${P}, with an initial angle of @${\alpha} and a final
angle of @${\alpha + \frac{\pi}{2}}, followed by a spiral centred at
@${P+\vec{v}}, with a radius of @${r\cdot f} and initial angle of @${\alpha
+ \frac{\pi}{2}}. The vector @${\vec{v}} will be positioned at @${P},
have a length of @${r\cdot(1-f)} and an angle @${\alpha + \frac{\pi}{2}}.

Obviously, this being a recursive process, it is necessary to define the
stopping condition, and there are (at least) two possibilities:

@itemlist[
@item{End when the number of circle quarters is zero.}
@item{End when the radius @${r} is lower to certain limit.}
@item{End when the angle @${\alpha} is higher than a certain limit.}
]

For now, let us consider the first possibility. According to the
described process, let us define the function that draws the spiral
with the following parameters: the @lisp[p] starting point, the
@lisp[r] initial radius, the @lisp[a] initial angle, the @lisp[n]
number of quarter circles, and the @lisp[f] reduction factor:

@def/no-results[
(define (spiral p r a n f)
  (if (= n 0)
    #t
    (begin
      (circumference-quarter p r a)
      (spiral (+pol p (* r (- 1 f)) (+ a pi/2))
               (* r f)
               (+ a pi/2)
               (- n 1)
               f))))
]

Note that the @fn[spiral] function is recursive, as it is defined
in terms of itself. Obviously, the recursive case is simpler
than the original case, since the number of circle quarters is
smaller, progressively approaching the stopping condition.

To draw the circumference quarter we will use Racket's @fn[arc] operation,
which receives the circumference centre and radius, and the initial and
final angles of the arc. To better understand this spiral
drawing process, let us also draw two lines starting at the
centre, outlining each circumference quarter. Later, once we have finished
developing these functions we will remove them.

@def/no-results[
(define (circumference-quarter p r a)
  (arc p r a pi/2)
  (line p (+pol p r a))
  (line p (+pol p r (+ a pi/2))))
]

Now we can try an example:

@def/no-results[
(spiral (xy 0 0) 3 (/ pi 2) 12 0.8)
]

The spiral drawn by the expression above is shown in @figref{fig:espiral1}.

@figure[#:tag "fig:espiral1"
	#:caption @elem{The drawn spiral.}]{
@(show-tikz 2)
@;tex{\centering\autotikz[0.5]{espiralA}}
}

The @fn[spiral] function allows us to define any number of spirals, but
with one restriction: each circular arc corresponds to an angle increment of
@${\frac{\pi}{2}}. Obviously, this function would be more useful if
this increment was also a parameter.

@figure[#:tag "fig:espiralDA"
	#:caption @elem{Spiral incremental angle as a parameter.}]{
@tex{\centering
  \begin{tikzpicture}[scale=0.5]
    \inputtikz{espiralDA}
  \end{tikzpicture}
}}

As can be deduced from observing @figref{fig:espiralDA}, the required
modifications are relatively trivial: there is only the need to add the
parameter @lisp[da], representing the angle increment @${\Delta_\alpha} of each
arc, and replace the occurrences of @${\frac{\pi}{2}} with this new parameter.
Naturally, instead of drawing a quarter of a circle, we will now have to draw a
circumference arc of angular amplitude @${\Delta_\alpha}. Seeing as the use of this parameter
also affects the meaning of the parameter @${n}, which will now 
represent the number of arcs with that amplitude, it is advisable
to explore a different stopping condition, based on the intended final angle
@lisp[fa]. However, we need to be mindful of one detail: the last arc may not be a
complete arc if the difference between the final and first angle exceeds
the angle increment. In this case the arc will only have
this difference for an angle value. The new definition is then:

@def/no-results[
(define (spiral p r a da fa f)
  (if (< (- fa a) da)
      (spiral-arc p r a (- fa a))
      (begin
        (spiral-arc p r a da)
        (spiral (+pol p (* r (- 1 f)) (+ a da))
                 (* r f)
                 (+ a da)
                 da
                 fa
                 f))))
]

The function that draws the arc is a generalization of the one that
draws one quarter of a circumference:

@def/no-results[
(define (spiral-arc p r a da)
  (arc p r a da)
  (line p (+pol p r a))
  (line p (+pol p r (+ a da))))
]

And now, to draw the same spiral as the one represented in
@figref{fig:espiral1}, we have to evaluate the following expression:

@lispcode[
(spiral (xy 0 0) 1 (/ pi 2) (/ pi 2) (* pi 6) 0.8)
]

Of course, now we can easily draw other spirals. The spirals produced
by the following expressions are shown in @figref{fig:espiral2}:

@def/no-results[
(spiral (xy 0 0) 2 (/ pi 2) (/ pi 2) (* pi 6) 0.9)

(spiral (xy 4 0) 2 (/ pi 2) (/ pi 2) (* pi 6) 0.7)

(spiral (xy 8 0) 2 (/ pi 2) (/ pi 2) (* pi 6) 0.5)
]

@figure[#:tag "fig:espiral2"
	#:caption @elem{Various spirals with different reduction coefficients: @${0.9}, @${0.7} and @${0.5}, respectively.}]{
@;tex{\centering\autotikz{espiralB}}
@(show-tikz 2)
}

Another possibility is to change the angle increment. The following
expressions test approximations to Sebastiano Serlio's approach (semi-circumferences),
Giuseppe Salviati's approach (circumference-quarters), and Guillaume Philandrier's approach
(circumference-eights) @footnote{Note that these are mere approximations. The original methods
were much more complex.}

@def/no-results[
(spiral (xy 0 0) 2 (/ pi 2) pi (* pi 6) 0.8)

(spiral (xy 4 0) 2 (/ pi 2) (/ pi 2) (* pi 6) 0.8)

(spiral (xy 8 0) 2 (/ pi 2) (/ pi 4) (* pi 6) 0.8)
]

The results are shown in @figref{fig:espiral3}.

@figure[#:tag "fig:espiral3"
	#:caption @elem{Several spirals with the coefficient reduction of @${0.8} and different angle increments:  @${\pi}, @${\frac{\pi}{2}}}]{
@;tex{\centering \autotikz{espiralC}}
@(show-tikz 2)
}

Finally, in order to compare the different spirals' construction
processes, we should adjust the reduction coefficient to the angle
increment, so that the reduction is applied to one whole turn and not
just to the chosen angle increment. Thus, we 
have the following expressions:

@def/no-results[
(spiral (xy 0 0) 2 (/ pi 2) pi (* pi 6) 0.8)

(spiral (xy 4 0) 2 (/ pi 2) (/ pi 2) (* pi 6) (expt 0.8 1/2))

(spiral (xy 8 0) 2 (/ pi 2) (/ pi 4) (* pi 6) (expt 0.8 1/4))
]

The results are shown in @figref{fig:espiral4}.

@figure[#:tag "fig:espiral4"
	#:caption @elem{Several spirals with the coefficient reduction of
 @${0.8} @emph{per turn} and with different angle increments:
 @${\pi}, @${\frac{\pi}{2}} and @${\frac{\pi}{4}}, respectively.}]{
@;tex{\centering \autotikz{espiralD}}
@(show-tikz 2)
}

@questions[
@question{
The @emph{gold spiral} is a spiral whose growth rate is @${\varphi},
with @${\varphi=\frac{1 + \sqrt{5}}{2}} being the @emph{golden
ratio}, popularized by Luca Pacioli in his 1509's work @emph{Divina
Proporzione}, although there are numerous accounts of its use many years before. 

The following drawing illustrates a golden spiral, with arcs inscribed in the correspondent @emph{golden
rectangle}, in which one side is @${\varphi} times bigger than the
smaller side.

@def/no-show[
(define (spiral-arc p r a da)
  (arc p r a da)
  (rectangle p (+pol p (* (sqrt 2) r) (+ a (/ da 2)))))

(spiral (xy 0 0) 9 0 (/ pi 2) (* pi 6) 0.618)
]

@centered{@(show-tikz 1 "very thick")}

As can be seen in the previous drawing, the golden rectangle has the
remarkable property of being recursively (and infinitely) decomposable
into a square and another golden rectangle.

Redefine the @fn[spiral-arc] function so that, in addition to
creating an arc, it also creates the enveloping square. Then write the
Racket's expression to create the golden spiral showned above.  }

@question{
An @emph{oval} is a geometrical figure that corresponds to the outline
of birds eggs. Given the variety of egg forms in Nature, is natural
to consider that there are many geometric ovals. The following figure
shows some examples, where some of the parameters that characterize the oval 
are systematically changed:

@def/no-show[
(define (egg p r0 r1 h)
  (define alpha (* 2 (atan (- r0 r1) (- h r0 r1))))
  (define  r2 (/ (- r0 (* r1 (cos alpha))) (- 1 (cos alpha))))
  (arc p r0 0 (- pi))
  (arc (+x p (- r0 r2)) r2 0 alpha)
  (arc (+x p (- r2 r0)) r2 (- pi alpha) alpha)
  (arc (+y p (* (- r2 r1) (sin alpha))) r1 alpha (- pi alpha alpha)))

(for/list ((r1 (in-range 0.3 0.9 0.1)))
  (for/list ((h (in-range 2.2 3.1 0.4)))
    (egg (xy (* r1 30) (* 8 h)) 1 r1 h)))
]

@;centered[@tex{\autodrawing{ovos}}]

@centered{@(show-tikz)}

An oval is composed of four circumference arcs as presented in the
following image:

@centered[@tex{
\begin{tikzpicture}[scale=0.7]
\inputtikz{ovulo}
\end{tikzpicture}}
]

The circular arcs needed to draw the oval are defined by the
radii @${r_0}, @${r_1}, and @${r_2}. Note that the circular arc of
radius @${r_0} covers an angle of @${\pi} and the circular arc of radius
@${r_2} covers an angle of @${\alpha}.

Define the @fn[oval] function that draws an egg. The function should
receive as parameters only the coordinates of the point @${P}, the @${r_0}
and @${r_1} radii, and the egg's height @${h}.}

@question{
Define the @fn[cylinder-pyramid] function that builds a pyramid of
cylinders piled on top of each other, as presented in the following
image. Note that the cylinders decrease in size (both in length and
radius) and suffer a rotation as they are being piled.

@fig[@autoimage{piramideCilindros}]
}
]

@section{Recursion in Nature}

Recursion is present in countless natural phenomena. Mountains,
for example exhibit irregularities that when observed at an
appropriate scale are identical to @ldots mountains. A river has
tributaries and each tributary is identical to @ldots a river. A
blood vessel has branches and each branch is identical to @ldots a
blood vessel. All these natural entities are examples of recursive
structures.

A tree is another good example of a recursive structure, since tree
branches are as small trees growing from the trunk. As it can be seen
in @figref{fig:arvores0}, in each tree branch there are other
small tree structures growing from them, a process which repeats itself
until it reaches a sufficiently small dimension for other structures to star to
appear, such as leaves, flowers, fruits, pine cones, etc.

@figure[#:tag "fig:arvores0"
	#:caption @elem{The recursive structure of trees. Photography by Michael
    Bezzina.}]{@authorizedPhoto{arvores/MichaelBezzina}}

If, in fact, a tree has a recursive structure then it should be
possible to "build" trees with recursive functions.  To test this
theory let us begin by considering a very simplistic version of a tree,
where we have a trunk is divided into two at a certain height. Each
of these sub-trunks grows in an angle with the trunk from which it
originated and reaches a certain length that is a fraction of the initial
trunk's length, as shown in @figref{fig:arvoreEsquema1}. The
stopping condition is reached when the length of the trunk becomes so small
that, instead of continuing to divide itself, another structure simply
appears. To simplify, let us designate the extremity of a branch with a leaf
that we will represent with a small circle.

@figure[#:tag "fig:arvoreEsquema1"
        #:caption @elem{Drawing parameters of a tree.}]{
@tex{\begin{tikzpicture}[scale=2]
\inputtikz{arvoreEsquema1}
\end{tikzpicture}}}

To give some dimension to the tree, let us consider that the @fn[tree] function
receives as arguments: the coordinate @${P} of the tree base, the
length @${c} of the trunk, and the angle @${\alpha} that it makes with its
origin. For the recursive phase, we will have as parameters the opening
angle difference @${\Delta_\alpha} that the new branch should make with the
previous one and the reduction coefficient @${f} for the trunk's
length. The first step is to compute the top of the trunk by using the
function @fn[+pol]. Next we draw the trunk from base to the top.
Finally, we test if the drawn trunk is sufficiently small. If it is, we
finish with the drawing of a circle centred at the top. Otherwise, we
make a double recursion to draw a sub-tree on the right and
another on the left. The definition of the function is then:

@def/no-results[
(define (tree p c a da f)
  (let ((top (+pol p c a)))
    (branch p top)
    (if (< c 2)
        (leaf top)
        (begin
         (tree top 
                 (* c f)
                 (+ a da)
                 da f)
         (tree top
                 (* c f)
                 (- a da)
                 da f)))))

(define (branch p0 p1)
  (line p0 p1))

(define (leaf p)
  (circle p 0.2))
]

A first example of a tree, created by the following code

@def/no-results[
(tree (xy 0 0) 20 (/ pi 2) (/ pi 8) 0.7)
]

is presented in @figref{fig:arvore1}.

@figure[#:tag "fig:arvore1"
        #:caption @elem{A "tree" measuring @${20} units, with the initial angle of @${\frac{\pi}{2}}, opening angle of @${\frac{\pi}{8}} and reduction coefficient of @${0.7}.}]{
	@(show-tikz 0.3 "ultra thick")
	@;tex{\autodrawing[0.5]{arvoreA}}
}

@figref{fig:arvores1} presents other examples where the
opening angle and the reduction coefficient vary. The sequence of
expressions that generated those examples is the following:

@def/no-results[
(tree (xy 0 0) 20 (/ pi 2) (/ pi 8) 0.6)

(tree (xy 100 0) 20 (/ pi 2) (/ pi 8) 0.8)

(tree (xy 200 0) 20 (/ pi 2) (/ pi 6) 0.7)

;(tree (xy 300 0) 20 (/ pi 2) (/ pi 12) 0.7)
]

@figure[#:tag "fig:arvores1"
	#:caption @elem{Multiple "trees" with different opening angles and reduction coefficient of the branches.}]{
@(show-tikz 0.1 "ultra thick")}

Unfortunately, the trees presented are "excessively" symmetrical: in
nature is literally impossible to find perfect symmetries. For this reason,
the model should be a little bit more sophisticated with the introduction
of different growth parameters for the branches on the left and on the right.
For that, Instead of having a single opening angle and only one length
reduction coefficient, we will apply two, as presented in @figref{fig:arvoreEsquema2}.

@figure[#:tag "fig:arvoreEsquema2"
	#:caption @elem{Design parameters of a tree with asymmetrical growth.}]{
@tex{\begin{tikzpicture}[scale=2]
\inputtikz{arvoreEsquema2}
\end{tikzpicture}
}}

The changes that we have to do to the @fn[tree] function to receive the new parameters
are trivial:

@def/no-results[
(define (tree p c a da0 f0 da1 f1)
  (let ((top (+pol p c a)))
    (branch p top)
    (if (< c 2)
        (leaf top)
        (begin
          (tree top
                  (* c f0)
                  (+ a da0)
                  da0 f0 da1 f1)
          (tree top
                  (* c f1)
                  (- a da1)
                  da0 f0 da1 f1)))))
]

The @figref{fig:arvores2} presents new examples of trees with
different opening angles and branch reduction coefficients on both
left and right sides, generated by the following expressions:

@def/no-results[
(tree (xy 0 0) 20 (/ pi 2) (/ pi 8) 0.6 (/ pi 8) 0.7)

(tree (xy 80 0) 20 (/ pi 2) (/ pi 4) 0.7 (/ pi 16) 0.7)

(tree (xy 150 0) 20 (/ pi 2) (/ pi 6) 0.6 (/ pi 16) 0.8)
]

@figure[#:tag "fig:arvores2"
	#:caption @elem{Various trees generated with different opening angles and length reduction coefficients for both left and right sides.}]{
@(show-tikz 0.1 "ultra thick")}

The trees generated by the @fn[tree] function are only a poor model
of reality. Although there are obvious signs that various natural
phenomena can be modelled by recursive functions, nature is not as
deterministic as our functions. So, in order to make our function
closer to reality, it is crucial that some randomness is
incorporated. This will be next section's subject.
}