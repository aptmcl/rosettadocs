#lang scribble/manual
@(require "../rosetta.rkt")
@setup-math

@title{State}

@section{Introduction}

As we have seen, Racket allows us to establish an association
between a name and an entity through the @stx[define] operator. In
this section, we will see that Racket also allows us to change this
association, causing the name to be associated with another
entity. This operation is called @emph{assignment} and it is done
through the @stx[set!] operator.

In this section we are going to discuss with more depth the concept of
assignment. For motivation we will start by introducing an important topic
where assignment plays a key role: randomness.

@section{Randomness}

Drawing involves making conscious decisions that lead to the intended
purpose. In this sense, designing appears to be a rational process where
there is no room for @emph{randomness}, @emph{luck} or
@emph{uncertainty}. In fact, for the drawings we have been doing so far,
rationality has been crucial because the computer requires
rigorous specification of what is intended, not allowing any
ambiguities. However, it is known that a design problem often requires
architects to try different solutions before finding the one that
pleases them. This experimentation requires some randomness in the process of
choosing the drawing "parameters". So, although the final design may
provide a structure that reflects the architect's rational intention,
the process that led to this final design is not necessarily rational
and may have gone through phases of ambiguity and uncertainty.

When architecture is inspired by nature, an additional factor of
randomness arises: in many cases, nature is intrinsically random. That
randomness, however, is not total and has restrictions. This fact is easily
understood when we consider that, although there are no two equal pine
trees, we are all capable of recognizing the pattern that
characterizes a pine tree. In all its master pieces, nature combines
regularity and randomness. In some cases, such as the growth of crystal,
there is more regularity than randomness. In other cases, such as the
behaviour of subatomic particles, there is more randomness than
regularly.

As in nature, this combination of randomness and regularity
is clearly visible in several works of modern architectural. As an example,
let us consider two important works by architect Oscar Niemeyer: the
Itamaraty Palace and the Mondadori Palace, seen in @figref{fig:#######}.
Despite the clear resemblances between them the first one excels for the
regularity in its arcade, as opposed to the evident randomness that
Niemeyer applied in the second one.

@figure[#:tag "fig:#######"
	#:caption @elem{Two works by Oscar Niemeyer. The first one, the Itamaraty Palace, designed in 1962. The second one, the Mondadori Palace, designed in 1968. Photographies by Bruno Kussler and Tristan Nitot, respectively.}]{
@authorizedPhoto[#:scale 0.48]{niemeyer/Itamaraty} @authorizedPhoto[#:scale 0.48]{niemeyer/Mondadori}}

If we want to use computers to design and this design assumes
randomness, then we must be able to incorporate it in our
algorithms. Randomness can be incorporated in various ways, for
example:

@itemlist[
@item{An object's colour may be chosen randomly.}
@item{The length of a wall can be a random value between certain limits.}
@item{The decision to divide an area or keep it whole can be random.}
@item{The geometric shape to apply to a given architectural element
can be chosen randomly.}
]

@section{Random Numbers}

In any of the above cases, we can reduce the randomness to choosing
numbers within certain limits. For a random colour, we can randomly
generate three numbers representing the RGB values for that colour.
@footnote{RGB stands for @emph{Red-Green-Blue}, a model where any colour
can be seen as the composition of the three primary colours red, green and
blue)} For random length, we can generate a random number within that length's
limits. For a logical decision, we can randomly generate an integer between
zero and one, making a choice if the number is zero, and another otherwise
if the number is one. For a random choice among a set of alternatives, we can
simply generate a random number between one and the number of elements
of the given set and then choose the alternative corresponding to the randomly
generated number.

These examples show us that what is essential is to be able to generate a 
random number within a certain range. Based on this operation we can
implement all the others.

There are two fundamental processes for generating random numbers. The
first process is based on the measurement of physical intrinsically
random processes, such as electronic noise or radioactive decay. The
second process is based on the use of arithmetic functions that, given
an initial value (called the @emph{seed}), produce a sequence
of seemingly random numbers, with each number of that sequence having been
generated based on the previous generated number. In this case we say it is
a @emph{pseudo-random number generator}. The term @emph{pseudo} is justifiable
since if we repeat the value of the original seed we will also repeat the
sequence of generated numbers.

Although a pseudo-random number generator produces a sequence of
numbers that are actually not random, it has two important advantages:

@itemlist[
@item{It can be easily implemented using any programming
language, not requiring any other additional mechanisms to obtain the randomness
source.}
@item{Often our programs contain bugs. In order to
identify the cause of that bug, it may be necessary to reproduce exactly
the same conditions that led to that bug. Besides that, after
the fixing the bug is often necessary to repeat the
program's execution again to make sure that bugs no longer occurs.
Unfortunately, when the program's behaviour depends on a truly
random sequence of numbers it is impossible to reproduce the exact
execution conditions: during the next execution the sequence of numbers
will almost certainly be different.}]

Therefore, from now on we will only consider generators of
pseudo-random numbers, which we will abusively designate as random
number generators. A generator of this kind is characterized by a function
@${f} that, given a certain argument @${x_i}, produces a number
@${x_{i+1}=f(x_i)}, @emph{apparently} unrelated to @${x_i}. The seed
of the generator is the element @${x_0} of the sequence.

What is left now is to find a suitable @${f} function. For this, and
among other qualities, it is required that the numbers generated by
@${f} be @emph{equally probable}, i.e., all numbers within a certain
range are equally probable to be generated and that the
@emph{period} of the sequence of generated numbers is as large as
possible, i.e., the sequence of numbers generated only starts
repeating after a long time.

@footnote{In Racket, the @fn[modulo] operator implements
the mathematical operation @${p\bmod q}, corresponding to the
remainder of the division of the first operand (the dividend, to the
left of the operator) by the second operand (the divisor, to the right
of the operator), having the same signal as the divisor. The operator 
@fn[remainder] implements the remainder of the division of the dividend
by the divisor, but with the same signal of the dividend.}

Numerous functions have been studied with these characteristics. The
most used is called the @emph{linear congruential generator function} with
the form:

@$${x_{i+1} = (a x_i + b)\bmod m}

For example, if we have @${a = 25173, b = 13849, m = 65536} and
we begin with any seed, for example, @${x_0=12345}, we obtain the
following pseudo-random numbers: 

2822, 11031, 21180, 42629, 27202, 49667, 50968, 33041, 37566, 43823,
2740, 43997, 57466, 29339, 39312, 21225, 61302, 58439, 12204, 57909,
39858, 3123, 51464, 1473, 302, 13919, 41380, 43405, 31722, 61131,
13696, 63897, 42982, 375, 16540, 25061, 24866, 31331, 48888, 36465,...
@footnote{Actually, this sequence is not random enough as there
is a pattern that repeats continuously. Can you discover it?}

We can easily confirm these results using Racket:

@def[
(define (next-random-number previous-random-number)
  (modulo (+ (* 25173 previous-random-number) 13849)
          65536))
]
@incremental[(next-random-number 12345)(next-random-number 2822)(next-random-number 11031)]

This approach, however, implies that we can only generate a "random"
number @${x_{i + 1}} if we recall the @${x_i} number generated
immediately before, so that we can provide it as an argument for the
@fn[next-random-number] function.  Unfortunately, the moment and the
point of the program at which we might need a new random number can occur
much later and much further than the moment and the point of the
program when the last random number was generated, which substantially
complicates the program's writing. It would be preferable that,
instead of having a @fn[next-random-number] function, that depends on
the previous @${x_i} generated value, we had a
@fn[random-number] function that did not need the previously
generated number to be able to produce the next one. This way, at any
point of the program where we might need to generate a new random
number, all we would have to do was to call the @fn[random-number]
function without having to recall the previously generated
number. Starting with the same seed value, we would have:

@def/no-show[
(define current-random-number 12345)

(define (random-number)
  (set! current-random-number
        (next-random-number current-random-number))
  current-random-number)
]

@incremental[(random-number)(random-number)(random-number)]

@section{State}

The @fn[random-number] function shows a different behaviour from the
functions we have seen so far. Until now, all the functions we have defined
behaved as traditional mathematical definition: given the
arguments, the function produces results and, more importantly, given
the same arguments the function @emph{always} produces the same
results. For example, regardless of the number of times the
@fn[factorial] function is called, for a given argument it will
always produce the factorial of this argument.

The @fn[random-number] function is different from the others because
besides not needing arguments, it produces a different result each time
it is called.

From a mathematical point of view, a function without parameters is not
uncommon: it is precisely what is known as a @emph{constant}. In fact,
just as we write @${\sin \cos x} to designate @${\sin(\cos(x))}, we can
just as well write @${\sin \pi} to designate @${\sin(\pi())}, where
@${\pi} can be seen as a function without arguments.

From a mathematical point of view, a function that produces different
results each time is called is anything but normal: is an abomination,
since according to the mathematical definition of a function this behaviour
is not possible. And yet, this is precisely the type of behaviour we would
like the @fn[random-number] function to have.

To obtain such behaviour is necessary to introduce the concept of
@emph{state}. We say that a function has state when its behaviour
depends on its history, i.e., its previous calls. The @fn[random-number]
function is an example of a function with state, but there are
countless other examples in the real world. A bank account also has
a state that depends on all past transactions. A car's fuel tank also
has state that depends on the fills and past journeys.

For a function to have history it must have @emph{memory}, i.e., it must
recall past events in order to influence future results. So far, we have
seen that the @stx[define] operator allowed us to @emph{define} associations
between names and values, but what has not yet been discussed is the
possibility to @emph{modify} these associations, i.e., change the value
that was associated to a particular name. For this, Racket
provides the @stx[set!] operator that given an already defined name and
given a new value to associate this name with, removes the previous
association and establishes the new one. It is this feature that allows us
to include memory in our functions.

In the @fn[random-number] function's case, we have seen that the memory
that is important to us is what the last random number
generated was. Let us thus suppose that we had an association between the
name @lisp[previous-random-number] and that number. Initially, this name
should be associated with the seed of the random number sequence. for that
we could define:

@def[
(define previous-random-number 12345)
]

Next, we can now define the @fn[random-number] "function" that, not only
uses the last value associated with that name, but also updates this
association with the new generated value:

@def[
(define (random-number)
  (set! previous-random-number
        (next-random-number previous-random-number))
  previous-random-number)
]

@incremental[(random-number)(random-number)]

As we can see, every time that the @fn[random-number] function
is called, the value associated with @lisp[previous-random-number]
is updated, influencing the function's future behaviour.  Obviously, at
any given moment, we can re-start the random numbers sequence simply by
restoring the seed value:

@incremental[(random-number)(random-number)
(set! previous-random-number 12345)
(random-number) (random-number) (random-number)]

@section{Random Choices} 

Observing the @fn[next-random-number] function definition we find that
its last operation is to compute the division's remainder by
@${65536}, which implies that the function always produces values
between @${0} and @${65535}. Although (pseudo) random,
these values are contained in a range that will only exceptionally be
useful. In fact, it is much more frequent that we need random numbers
that are contained in much smaller intervals. For example, if we want to
simulate the action of flipping a coin, we are only interested in
having the random numbers @${0} or @${1}, representing "heads" or "tails".

Just as our random numbers are limited to the range @${[0,65536[} by having
the remainder of the division by @${65536}, we can once again apply the same
operation to produce a smaller range. Therefore, in the case of flipping
a coin, we can simply use the @fn[random-number] function to generate a
random number and then apply to it the division remainder by two. In
general, when we want random numbers in an interval @${[0,x[}, we apply
the division remainder by @${x}. Thus we can define a new function
that generates a random number between zero and a parameter, and which
we will, by tradition, call @fn[random]:

@def[
(define (random x)
  (remainder (random-number) x))
]

Note that the @fn[random] function should never receive an argument
greater than @${65535} because that would make the function lose the
@emph{equiprobability} feature of the generated numbers: all numbers
greater than @${65535} will have zero probability of occurring.
@footnote{In fact, the upper limit should be fairly below the limit of
the @fn[random] function to maintain the equiprobability of the results.}

It is now possible to simulate a number of random phenomena such as, for
example, flipping a coin:

@def[
(define (heads-or-tails)
  (if (= (random 2) 0)
    "heads"
    "tails"))   
]

Unfortunately, when repeatedly tested, our function does not seem very
random:


@incremental[(heads-or-tails) (heads-or-tails) (heads-or-tails) 
(heads-or-tails) (heads-or-tails) (heads-or-tails)]

In fact, the results we are getting are a constant repetition of the
pair: @lisp[heads]/@lisp[tails], which shows that the expression
@lisp[(random 2)] merely generates the following sequence:

0101010101010101010101010101010101010101010101010101010101

The same phenomenon occurs for other intervals. For example, the
@lisp[(random 4)] expression should generate a random number from the
set @${\{0,1,2,3\}} but its repeated invocation generates the
following sequence of numbers:

0123012301230123012301230123012301230123012301230123012301

In spite of the numbers perfect equiprobability they are clearly not
random.

The problem in both previous sequences is the fact that they have a
very small @emph{period}. The period is the number of generated
elements before the cycle is restarted, repeating the same elements
previously generated. Obviously, the greater the period the better
the random numbers generator will be and therefore the generator
that we have shown is of poor quality.

Great amounts of effort have been invested on finding good random number
generators and although the better ones are produced using fairly more
sophisticated methods than the ones we have used so far, it is also possible
to find a good linear congruential generator as long we choose the
parameters wisely. In fact, the linear congruential generator @$${x_{i+1}
= (a x_i + b) \bmod m} can be a good pseudo-random generator
provided we have @${a=16807}, @${b=0} and @${m=2^{31}-1=2147483647}. A
direct translation of the mathematical definition to Racket produces
the following function:

@def[
(define (next-random-number previous-random-number)
  (modulo (* 16807 previous-random-number)
          2147483647))
]

Using this new definition, the repeated evaluation of the
@lisp[(random 2)] expression produces the following sequence:

01001010001011110100100011000111100110110101101111000011110

and the evaluation of the @lisp[(random 4)] expression produces:

21312020300221331333233301112313001012333020321123122330222

It is fairly clear that the generated sequences now have a period large
enough for any repetition pattern to be detected. Thus, from now on,
this will be the definition of the @fn[next-random-number] function that
will be used.


@subsection{Random Fractional Numbers}

The process of generating random numbers that we have implemented is
only able to generate random integer numbers. However, we often also
need to generate fractional random numbers, for example, in the interval
@${[0,1[}.

In order to fulfil these two requirements it is usual in Racket, that
the @fn[random] function receives the generator's upper limit @${x}
and analyses it in order to determine whether it is integer or real,
returning an appropriate random value in each case. For this we need
nothing more than to map the integers' generator interval, which is as
we have seen @${[0,2147483647 [}, in the interval @${[0,x[}. The
implementation is trivial: @footnote{This function uses the
@fn[inexact?] universal recognizer which is defined in the
@ref{sec:reconhecedores} section.}

@def[
(define (random x)
  (if (inexact? x)
      (* x (/ (random-number) 2147483647.0))
      (remainder (random-number) x)))
]

@subsection{Random Numbers within a Range}

If instead of generating random numbers in the range @${[0, x[} we
prefer to generate random numbers in the range @${[x_0, x_1[}, then we
just have to generate a random number within @${[x_0, x_1[} and add
@${x_0}. The @fn[random-range] function implements this behaviour:


@def[
(define (random-range x0 x1)
  (+ x0 (random (- x1 x0))))
]

Like the @fn[random] function, the @fn[random-range] function
also produces a real value when limits are real numbers.

As an example, let us consider the @fn[tree] function that models a
tree, defined as:

@def[
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

@def/no-show[
(define (branch p0 p1)
  (line p0 p1))

(define (leaf p)
  (circle p 0.2))
]

To incorporate some randomness in this tree model we can consider that
the branches opening angles and the length reduction factors of those
branches are not constant values throughout the recursion process, varying
within certain limits. Thus, instead of worrying about having
different opening angles and factors for the left and right branches,
we will simply have random variations on both sides:

@def[
(define (tree p c a min-a max-a min-f max-f)
  (let ((top (+pol p c a)))
    (branch p top)
    (if (< c 2)
        (leaf top)
        (begin
          (tree top
                  (* c (random-range min-f max-f))
                  (+ a (random-range min-a max-a))
                  min-a max-a min-f max-f)
          (tree top
                  (* c (random-range min-f max-f))
                  (- a (random-range min-a max-a))
                  min-a max-a min-f max-f)))))
]

Using this new version we can generate several similar trees yet
sufficiently different to seem much more natural. Trees shown in
@figref{fig:arvoresRandom7} were generated using exactly the same
growth parameters:

@def/no-results[
(tree (xy   0   0) 20 (/ pi 2) (/ pi 16) (/ pi 4) 0.6 0.9)
(tree (xy 150   0) 20 (/ pi 2) (/ pi 16) (/ pi 4) 0.6 0.9)
(tree (xy 300   0) 20 (/ pi 2) (/ pi 16) (/ pi 4) 0.6 0.9)
(tree (xy   0 150) 20 (/ pi 2) (/ pi 16) (/ pi 4) 0.6 0.9)
(tree (xy 150 150) 20 (/ pi 2) (/ pi 16) (/ pi 4) 0.6 0.9)
(tree (xy 300 150) 20 (/ pi 2) (/ pi 16) (/ pi 4) 0.6 0.9)
]

@figure[#:tag "fig:arvoresRandom7"
	#:caption @elem{Multiples trees created with branches opening angles between @${[\frac{\pi}{2},\frac{\pi}{16}[} and length reduction factors within @${[0.6,0.9[}.}]{
 @(show-tikz 0.05 "ultra thick")
}

@questions[
@question{ The trees produced by the @fn[tree]
function are unrealistic because they are completely two-dimensional,
with trunks that are simple lines and leaves that are small circles.
The calculation of the branches' and leaves' coordinates is also
two-dimensional, relying on polar coordinates given by the
@lisp[width] and @lisp[angle] parameters.

It is intended that you redefine the @fn[branch], @fn[leave] and
@fn[tree] functions in order to increase the realism of the
generated trees.

To simplify, assume that the leaves can be simulated by
small spheres and branches can be simulated by truncated cones with
the base radius being @${10\%} of the branch's length and top radius
being @${90\%} of the base radius.

For the generation of trees to be truly three-dimensional, redefine the
@fn[tree] function so that the top of each branch is a point in
spherical coordinates chosen randomly from the base of the
branch. This implies that the tree function, instead of having two
parameters for length and angle of the polar coordinates, it will need
to have three, for length, longitude and co-latitude. Likewise, instead
of receiving the four limits for generating random lengths and angles
it will need six limits for the three parameters.

Try different arguments in your redefinition of the @fn[tree]
function, in order to create something similar to the image below:

@centered[@tex{\autoimage{arvores3D}}]
}

@question{ Define a function named @fn[random-cylinder] that receives as
argument a number of cylinders @${n} and that generates @${n} cylinders
placed in random positions, with random radii and lengths, as presented
in the following image:

@centered[@tex{\autoimage{cilindrosAleatorios}}]
}

@question{ A @emph{random path} is a formalization of a particle
motion that is subjected to impulses of random intensity from random
directions. This phenomenon happens. for example, to a grain of pollen
when falling into water: as the molecules of water bump into the grain,
it will randomly move.

The following image present three random paths:

@centered[
@tex{
\pgfmathsetseed{1}
\hfill
\foreach \l/\n in {5/100,2/200,8/50}
{
  \begin{tikzpicture}[x=10pt,y=10pt,thick]
    \coordinate (current point) at (0,0);
    \foreach \i in {0,1,...,\n}
    {
      \pgfmathrandominteger{\r}{0}{\l}
      \pgfmathrandominteger{\f}{0}{360}
      \draw(current point)--++(\f :\r mm)coordinate(current point);
    }
  \end{tikzpicture}\hfill
}
}]

Consider a model of a @emph{random path} in a two-dimensional
plane. Define the @fn[random-path] function that has as parameters
the starting point @${p} of the path, the maximum distance @${d} that
particle can move when pushed and the @${n} number of successive
impulses that the particle will receive. Note that in arch impulse
the particle moves in a random direction, a random distance between
zero and the maximum distance. This function should simulate the
corresponding random path, as presented in the previous figure, which
was created with three different executions of this function. From left to
right, the following parameters were used: @${d=5, n=100}, @${d=2,
n=200}, and @${d=8, n=50}. }


@question{ Define a @emph{biased} @fn[heads-or-tails] function such
that, even though it randomly produces the @emph{string} @fn["heads"] or
@fn["tails"], "heads" falls only @${30\%} of the times the function is executed.  }

@question{ Define the @fn[cylinder-sphere] function that given a point
@${p}, a length @${l}, a radius @${r} and a number @${n},
creates @${n} cylinders, of length @${l} and radius @${r}, which the base
centred at @${p} point, with the top positioned randomly, as presented in
the following image:

@centered[@tex{\autoimage[0.5]{cilindrosEsfera}}]
}

@question{ Define a function called @fn[connected-blocks] that is
able to build a cluster of connected blocks, i.e., blocks are
physically linked together, as presented in the following figure:

@centered[@tex{\autoimage[0.7]{blocosConexos}}]

Note that the adjacent blocks always have orthogonal orientations.
}
]

@section{Urban Planning}

Cities are a good example of the combination between structure and
randomness. Although many ancient cities appear to be chaotic as a
result of their non planned development, the truth is that since the
early ages there was the felt need to structure cities in order to
facilitate its use and organize its growth, with well known examples of
planed cities dating from @${2600} BC.

Although there are many different approaches, the two most usual ways of
planning a city are through the orthogonal or circular plan. In the
orthogonal plan, the avenues are straight lines and make right
angles between them. In the circular plan, the main avenues converge into
a central square and the secondary avenues develop along concentric
circles around this main square, following the city's growth.
@Figref{fig:planoOrtogonal} presents a good example of a city mainly
developed in an orthogonal plan. In this section we will explore
randomness in order to produce variations in these plans.

@figure[#:tag "fig:planoOrtogonal"
	#:caption @elem{New York's aerial view in the USA. Photography by Art Siegel.}]{
  @authorizedPhoto{cidade/artolog}
}

In an orthogonal plan, the city is organized into blocks, with each block
assuming a rectangular or square shape and can contain several buildings.
To simplify, we will assume that each block will be squared shape and
only contains a single building. Each building will have a width
determined by the block's width and an imposed height. The buildings
will be separated from each other by streets with a fixed width. The
model of this city is presented in @figref{fig:esquemaCidade}.

@figure[#:tag "fig:esquemaCidade"
	#:caption @elem{Diagram of an orthogonal city plan.}]{
@tex{
\begin{tikzpicture}[scale=1.5]
  \inputtikz{cidade}
  \end{tikzpicture}
}}

In order to structure the function that creates the city with the
orthogonal plane let us first decompose the process in the
construction of the successive streets. The construction of each street
is then be decomposed in the successive construction of buildings. Thus,
we must parametrize the function with the number of streets and the
number of buildings per street. In addition to that we will need to know
the coordinates of the city's origin point @${p}, the width @${w} and
height @${h} of each building and finally the width @${s} of the street.
The function will create a row of buildings followed by a street and,
recursively, create the remaining rows of buildings and streets
corresponding to the next row. To simplify, we will assume that streets
are aligned with the @${x} and @${y} axes, whereby each new road
corresponds to a displacement along the @${y} axis and each new building
corresponds to a displacement along the @${x} axis.  Thus we will have:

@def[
(define (buildings-grid p n-streets m-buildings w h s)
  (if (= n-streets 0)
    #t
    (begin
      (street-buildings p m-buildings w h s)
      (buildings-grid (+y p (+ w s))
                     (- n-streets 1)
                     m-buildings
                     w
                     h
                     s))))
]

For the construction of streets with buildings, the process is the
same: we place a building at the correct coordinates and then recursively
we place the remaining buildings after the corresponding displacement.
The following function implements this process:

@def[
(define (street-buildings p m-buildings w h s)
  (if (= m-buildings 0)
    #t
    (begin
      (building p w h)
      (street-buildings (+x p (+ w s))
                   (- m-buildings 1)
                   w
                   h
                   s))))
]

Finally, we need to define a function that creates a building. To
simplify, we modelled it as a parallelepiped:

@def[
(define (building p w h)
  (box p w w h))
]

With these three functions we can now try to build a new
urbanization. For example, the following expression creates a set of
ten streets with ten buildings per street:

@lispcode[
(buildings-grid (xyz 0 0 0) 10 10 100 400 40)
]

The result is presented in @figref{fig:cityrender1}.

@figure[#:tag "fig:cityrender1"
	#:caption @elem{An urbanization in an orthogonal grid with a hundred buildings all with the same height.}]{
@tex{\autoimage{cidadeA}}
}

As it is obvious from @figref{fig:cityrender1} the produced
urbanization is not very elegant as it lacks some of the randomness that
characterizes cities.

To incorporate this randomness we start by considering that the
buildings' height can randomly vary from a maximum height to one-tenth
of that height. For this, we redefine the @fn[building] function:

@def[
(define (building p w h)
  (box p
       w
       w
       (* (random-range 0.1 1.0) h)))
]

With the exact same parameters as before in two consecutive calls of the
same expression we can now build more appealing urbanizations shown
in @figref{fig:cityrender0} and @figref{fig:cityrender2}.

@figure[#:tag "fig:cityrender0"
	#:caption @elem{An urbanization in an orthogonal grid with a hundred buildings with random heights.}]{
	@tex{\autoimage{cidadeB}}
}

@figure[#:tag "fig:cityrender2"
	#:caption @elem{An urbanization in an orthogonal grid with a hundred buildings with random heights.}]{
@tex{\autoimage{cidadeC}}
}

@questions[
@question{ The urbanizations produced by the
previous functions do not present sufficient variability as all the
buildings have the same shape. To improve the realism and quality of
the urbanization, it is intended to set different functions for
different types of buildings: the @fn[building0] function should
build a parallelepiped with random height as before and
the @fn[building1] function should build a cylindrical tower with a
random height, contained within the limits of a block. Define these two
functions. }

@question{ Use both @fn[building0] and @fn[building1] functions defined
in the previous exercise to redefine the @fn[building] function so that
it randomly constructs different buildings. The resulting urbanization
should be composed of @${20\%} circular towers and @${80\%}
rectangular buildings, as illustrated in the following figure:

@centered[@tex{\autoimage{cidadeD}}]
}

@figure[#:tag "fig:prediosBlocos"
        #:caption @elem{View of Manhattan. Photography of James K. Poole.}]{
  @authorizedPhoto{cidade/jameskpoole-borderless}}

@question{ The cities created in previous exercises only allow the
creation of two types of buildings: rectangular and cylindrical
buildings. However, when we observe an real city as shown in 
@figref{fig:prediosBlocos} we find that there are buildings with many
other shapes meaning that in order to increase the realism of our
simulations we will need to implement a larger number of functions, each
one able to create a different building.

Fortunately, a careful observation of @figref{fig:prediosBlocos} shows
that, in fact, many of the buildings follow a pattern and can be modelled
by overlapping parallelepipeds with random dimensions but always
successively smaller depending on the building's height. This is something
that we can easily implement with a single function.

Consider that this building is parametrized by the number of
intended "blocks" to overlap, the coordinates of the first block and by
the length, width and height of the building. The base block has
exactly the specified length and width but its height should be
between @${20\%} and @${80\%} of the total building height. The
following blocks are centred on the block immediately below, with a
length and width that are between @${70\%} and @${100\%}
of the corresponding parameters of the block immediately below. The
height of these blocks should be between @${20\%} and @${80\%} of the
remaining height of the building. The following image shows some
examples of buildings that follow this model:

@centered[@tex{\autoimage{cidadeE}}]

Based on this specification, define the @fn[building-blocks]
function and use it to redefine the @fn[building0] function so that
the generated buildings have a random number of overlapping
blocks. With this redefinition, the @fn[buildings-grid] function
should be able to generate a model similar to the following image, in
which for each building the number of blocks varies between @${1} and
@${6} was used:

@centered[@tex{\autoimage{cidadeF}}]
}

@question{ Generally, cities have a core of relatively high buildings
in the usually called @emph{business centre} (abbreviated to CBD, an
acronym of Central Business District), and as we move away the height
tends to decrease, a phenomenon clearly visible in @figref{fig:planoOrtogonal}.

The variation of the buildings height can be modelled by several
mathematical functions but in this exercise it is intended that the
two-dimensional Gaussian distribution is applied, given by:

@$${f(x,y) = e^{-\left( (\frac{x-x_o}{\sigma_x})^2 +
      (\frac{y-y_o}{\sigma_y})^2\right)}}

In which @${f} is the weighting height factor, the @${(x_0, y_0)} are
the coordinates of the highest point of the Gaussian surface, and
@${\sigma_0} and @${\sigma_1} are the factors that determinate the
two-dimensional stretch of this surface. In order to simplify,
assume that the business centre is located at @${(0,0)} and that
@${\sigma_x=\sigma_y=25 l}, with @${l} being the building's width. The
following image shows an example of one such urbanization:

@centered[@tex{\autoimage{cidadeG}}]

Incorporate this distribution in the city generation process to produce
more realistic cities. }

@question{ Cities often have more than one nucleus of high
buildings. These nuclei are separated from each other by zones of less
tall buildings. As in the previous exercise, each core of buildings
can be modelled by a Gaussian distribution.  Assuming that the
multiple cores are independent from each other, the buildings' height
can be modelled by overlapping Gaussian distributions, i.e., at
each point, the buildings' height is the maximum height of the
Gaussian distributions of the multiple cores.

Use the previous approach to model a city with three cores of
"skyscrapers", similar to the one presented in the following image:

@centered[@tex{
    \autoimage{cidadeH}
}]
}

@question{ It is intended to create a set of @${n} tangent spheres to
a virtual sphere centred at @${p}, with a limit radius @${R_L}. The
spheres are centred at a random distance of @${p} which is a random
value between an inner radius @${r_i} and an outer radius @${r_e}, as
presented in the following scheme:

@centered[@tex{
  \begin{tikzpicture}[scale=3]
    \inputtikz{esferasTangentes}
  \end{tikzpicture}
}]

Define a function called @fn[spheres-in-sphere] that from the centre
@${p}, the radii @${r_i}, @${r_e} and @${r_l} and also a @${n}
number, creates this set of spheres, able to produce images such
as the following:

@centered[@tex{
  \autoimage[0.3]{esferasNaEsfera0}\hfill
  \autoimage[0.3]{esferasNaEsfera1}\hfill
  \autoimage[0.3]{esferasNaEsfera2}
}]
}
]