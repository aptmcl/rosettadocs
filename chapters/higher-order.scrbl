#lang scribble/manual
@(require "../rosetta.rkt")
@setup-math

@title{Higher-Order Functions}

@section{Introduction}

We have been demonstrating how the Racket language can enable us,
through the definition of appropriate functions, to create the
architectural forms that we have in mind.  One of the advantages of
the definition of these functions is that many of these architectural
forms become @emph{parameterized}, allowing us to easily try
variations until we reach the numerical values of the parameters that
satisfy us.

Despite the "infinite" variability that the parameters allow us, there
will always be limits to what we can do with @emph{only} numerical
parameters, and for us to have a superlative degree of variability, we
will also have to vary the actual functions.

In this section we will discuss @emph{higher-order} functions. A
higher-order function is a function that receives functions as
arguments or a function that returns functions as result. Apart from
this feature a higher-order function is no different from any other
function.

@section{Curvy Facades}

To motivate the discussion let us consider a simple problem: we intend
on idealizing a building where three of the sides are planar and the
fourth --- the facade --- is a curvilinear vertical surface. To start,
we can consider that this curvilinear surface has a sinusoidal shape.

As seen in section @ref{sec:extrusions}, a sinusoid is determined
by a set of parameters, such as the @emph{amplitude} @${a}, the number
of cycles per length unit @${omega} and the @emph{phase} @${phi} of
the curve in relation to the @${y} axis. With these parameters the
sinusoid curve equation has the form: @${y(x) = a \sin(\omega x +
\phi)}

To calculate the points of a sinusoid we can use the function
@fn[sinusoid-points] that computes a list of coordinates @${(x,y)}
corresponding to the evolution of a sinusoid between the limits of a
range @${[x_0, x_1]}, with an increment @${\Delta_x};

@def[
(define (sinusoid-points p a omega fi x0 x1 dx)
  (if (> x0 x1)
    (list)
    (cons (+xy p x0 (sinusoid a omega fi x0))
          (sinusoid-points p a omega fi (+ x0 dx) x1 dx))))
]


The previous function generates a list with the point of the facade. In order
to model the rest of the building we need to join these points to form
a curve and we need to join the three rectilinear sides in order to form a region
that corresponds to the building floor plan. For this, we will connect the points with a
@emph{spline} and form a region between it and the
lines that delimit the other three sides of the building. The length
@${l_x} of the building will be given by the horizontal distance
between the two ends of the curve of the facade. The
width @${l_y} and height @${l_z} will have to be parameters. Thus, we
have:


@def[
(define (building points ly lz)
  (let ((p0 (car points))
        (p1 (last points)))
    (let ((lx (- (cx p1) (cx p0))))
      (extrusion
        (surface
          (spline points)
          (line p0 (+xy p0  0 ly) (+xy p0 lx ly) p1))
       lz))))
]

To visualize the capabilities of the previous function we can build
several buildings using different values for the sinusoid's
parameters. In the following expressions we considered two rows of
buildings, all with @${15} meters long, @${10} meters wide and @${20}
or @${30} meters tall, depending on the row. @Figref{fig:edificioSinusB}
shows these buildings for different values of the
parameters @lisp[a], @lisp[omega] and @lisp[fi]:

@figure[#:tag "fig:edificioSinusB"
        #:caption @elem{Urbanization of buildings of which the facade corresponds to sinusoid walls with different parameters.}]{
@autoimage{urbeSinus}
}


@lispcode[
(building (sinusoid-points (xy  0  0) 0.75 0.5 0 0 15 0.4) 10 20)
(building (sinusoid-points (xy 25  0) 0.55 1.0 0 0 15 0.2) 10 20)
(building (sinusoid-points (xy 50  0) 0.25 2.0 0 0 15 0.1) 10 20)
(building (sinusoid-points (xy  0 20) 0.95 1.5 0 1 15 0.4) 10 30)
(building (sinusoid-points (xy 25 20) 0.85 0.2 0 0 15 0.2) 10 30)
(building (sinusoid-points (xy 50 20) 0.35 1.0 0 1 15 0.1) 10 30)
]

Unfortunately, although the @fn[sinusoid-points] function is very
useful for modelling a multitude of different buildings with a
sinusoidal facade, it is totally useless to model buildings
of which the facade follows a parabola or a logarithm or an exponential or,
in fact, any other curve which is not reducible to a sinusoid. In
fact, although the adjustment of the parameters allows us infinite
variability of the modelled building, even then it will always be a particular
case of a sinusoidal facade.

Naturally nothing prevents us from defining other functions for modelling
different facades. Let us imagine, for example, that we want a facade with
the curvature of a parabola. The parabola with vertex at @${(x_v,y_v)}
and focus point at @${(x_v,y_v+d)}, with @${d} being the distance from the vertex
to the focus point, is defined by the following equation:

@$${(x - x_v)^2 =  4d(y - y_v)}

that is

@$${y=\frac{(x - x_v)^2}{4d}+y_v}

In Racket, this function is defined as:

@def[
(define (parabola xv yv d x)
  (+ (/ (expt (- x xv) 2)
        (* 4 d))
     yv))
]

In order to generate the parabola's points we have, as usual, to
iterate over a range @${[x_0, x_1]} with a certain increment
@${Delta_x}:

@def[
(define (parabola-points p xv yv d x0 x1 dx)
  (if (> x0 x1)
    (list)
    (cons (+xy p x0 (parabola xv yv d x0))
          (parabola-points p xv yv d (+ x0 dx) x1 dx))))
]

The following expressions test this function with different values of
the various parameters:

@lispcode[
(building (parabola-points (xy  0  0) 10  0  5 0 15 0.5) 10 20)
(building (parabola-points (xy 25  0)  5  0  3 0 15 0.2) 10 20)
(building (parabola-points (xy 50  0)  7  1 -2 0 15 0.1) 10 20)
(building (parabola-points (xy  0 20)  8  0  2 0 15 0.4) 10 30)
(building (parabola-points (xy 25 20)  6 -2  3 0 15 0.2) 10 30)
(building (parabola-points (xy 50 20)  5  0  6 0 15 0.1) 10 30)
]

generating the "urbanization" represented in @figref{fig:edificioParabola}.

@figure[#:tag "fig:edificioParabola"
        #:caption @elem{Urbanization of buildings of which the facade corresponds to paraboloidal walls with different parameters.}]{
@autoimage{urbeParabola}
}

Once more, the parametrization of the function
@fn[parabola-points] allows us to generate an infinity of buildings
of which the facade has the curvature of a parabola, but they will always be
different buildings from those that we can create with the @fn[sinusoid-points]
function. Although the modelling process used is absolutely
identical in both cases, the applied base functions---@fn[sinusoid]
@emph{vs} @fn[parabola] --- will always produce different curves,
regardless of the particular parameters used for each one.

Clearly, if we now want to create buildings with a curved facade
determined by a function @${f(x)} that is neither a sinusoid nor a
parabola, we can not employ the previous functions. First, we will need
to define the function @lispemph[f] that implements that curve and,
second, we will need to define the @lispemph[f-points] function
that generates the points of @lispemph[f] in an interval.
This second part seems overly repetitive and, indeed, it
is. We simply have to observe the @fn[sinusoid-points] and
@fn[parabola-points] functions to conclude that they are very similar
to each other. Logically, the same will happen with the new
@lisp[f-points] function.

This repetition leads us to consider the possibility of abstracting
the processes in question so that we are not obliged to repeat
definitions that only vary in small details. For this, we can start by
comparing the definitions of the functions and understand where the
differences are. For example, in the case of the
@fn[sinusoid-points] and @fn[parabola-points] functions, we
have:


@lispcode[
(define (#,(lispemph sinusoid-points) p #,(lispemph a) #,(lispemph omega) #,(lispemph fi) x0 x1 dx)
  (if (> x0 x1)
    (list)
    (cons (+xy p x0 (#,(lispemph sinusoid) #,(lispemph a) #,(lispemph omega) #,(lispemph fi) x0))
          (#,(lispemph sinusoid-points) p #,(lispemph a) #,(lispemph omega) #,(lispemph fi) (+ x0 dx) x1 dx))))
]

@lispcode[
(define (#,(lispemph parabola-points) p #,(lispemph xv) #,(lispemph yv) #,(lispemph d) x0 x1 dx)
  (if (> x0 x1)
    (list)
    (cons (+xy p x0 (#,(lispemph parabola) #,(lispemph xv) #,(lispemph yv) #,(lispemph d) x0))
          (#,(lispemph parabola-points) p #,(lispemph xv) #,(lispemph yv) #,(lispemph d) (+ x0 dx) x1 dx))))
]

We marked in italic the differences between the two functions, and as
we can see, the differences are in the parameters' names and the name
of the function that is invoked: @fn[sinusoid] in the first case and
@fn[parabola] in the second. As the names of the parameters of
a function are only visible in the function's body (i.e., are
@emph{local} to the function), we can rename them as long as we do it
consistently. This means that the following definitions would have
exactly the same behaviour:

@lispcode[
(define (#,(lispemph sinusoid-points) p #,(math-in "\\alpha") #,(math-in "\\beta") #,(math-in "\\gamma") x0 x1 dx)
  (if (> x0 x1)
    (list)
    (cons (+xy p x0 (#,(lispemph sinusoid) #,(math-in "\\alpha") #,(math-in "\\beta") #,(math-in "\\gamma") x0))
          (#,(lispemph sinusoid-points) p #,(math-in "\\alpha") #,(math-in "\\beta") #,(math-in "\\gamma") (+ x0 dx) x1 dx))))
]

@lispcode[
(define (#,(lispemph parabola-points) p #,(math-in "\\alpha") #,(math-in "\\beta") #,(math-in "\\gamma") x0 x1 dx)
  (if (> x0 x1)
    (list)
    (cons (+xy p x0 (#,(lispemph parabola) #,(math-in "\\alpha") #,(math-in "\\beta") #,(math-in "\\gamma") x0))
          (#,(lispemph parabola-points) p #,(math-in "\\alpha") #,(math-in "\\beta") #,(math-in "\\gamma") (+ x0 dx) x1 dx))))
]

It now becomes absolutely clear that the only difference between the two
functions lies in an invocation they make, which is @fn[sinusoid] in one
case and @fn[parabola] in the other.

Now, when two functions differ only in a name they use
internally, it is always possible to define a third function that
generalizes them, simply transforming that name into an additional
parameter.

Suppose then that we make the following definition, where @lisp[f] is
this additional parameter:

@def[
(define (function-points p f alpha beta gamma x0 x1 dx)
  (if (> x0 x1)
    (list)
    (cons (+xy p x0 (f alpha beta gamma x0))
          (function-points p f alpha beta gamma (+ x0 dx) x1 dx))))
]

The innovative look of the @fn[function-points] function lies in the fact that
it receives a function as an argument, a function which will be
associated to the parameter @lisp[f]. When, in the body of the
@fn[function-points] function, a call is made of the @lisp[f] function, we
are, actually, calling the function that has been passed as argument to
the @lisp[f] parameter.

Thus, the expression

@lispcode[
(sinusoid-points p a omega fi 0 lx dx)
]

is absolutely identical to

@lispcode[
(function-points p sinusoid a omega fi 0 lx dx)
]

Similarly, the expression

@lispcode[
(parabola-points p xv yv d 0 lx dx)
]

is absolutely identical to

@lispcode[
(function-points p parabola xv yv d 0 lx dx)
]

More important than the fact that we can dismiss the
@fn[sinusoid-points] and @fn[parabola-points] functions is the fact that,
now, we can model buildings of which the facades follow any other curve we
want. For example, the function that describes the
damped oscillatory motion has the definition:

@$${y=ae^{-bx}\sin(cx)}

or, in Racket:

@def[
(define (damped-oscillatory a b c x)
  (* a (exp (- (* b x))) (sin (* c x))))
]

Using the @fn[function-points] function, it is now trivial to define a
building of which the facade follows the curve of the damped oscillatory
motion. For example, the following expression

@lispcode[
(building
 (function-points (xy 0 0)
		damped-oscillatory
		-5 0.1 1
		0 40 0.4)
 10
 20)
]

produces, as a result of its evaluation, the building presented in
@figref{fig:edificioOscilatorioAmortecido}.

@figure[#:tag "fig:edificioOscilatorioAmortecido"
        #:caption @elem{A building with a facade that follows the curve of the damped oscillatory motion.}]{
@autoimage[#:scale 0.7]{urbeOscilatorio}
}


@section{Higher-Order Functions}

The @fn[function-points] function is an example of a class of
functions which we call @emph{higher-order}. A higher-order function
is a function that receives other functions as arguments or returns
other functions as result. In the case of the @fn[function-points]
function it receives a function of a parameter that will be called in
successive interval points, producing the list of the found
coordinates.

Higher-order functions are important tools for abstraction. They allow
the abstraction of computations in which there is a part of the computation
that is common and one (or more parts) that vary from case to
case. Using a higher-order function, we only implement the part of the
computation that is common, leaving the variable parts of the computation to
be implemented as functions that will be passed in the parameters of
the higher-order function.

The concept of higher-order function exists for a long time in
mathematics, although rarely explicitly mentioned. Let us
consider, for example, a function which sums the squares of all
integers between @${a} and @${b}, @${\sum_{i=a}^{b}i^2}:


@def[
(define (square-sum a b)
  (if (> a b)
    0
    (+ (sqr a) 
       (square-sum (+ a 1) b))))
]

@incremental[(square-sum 1 4)]

Let us now consider another function that sums the square roots of all
the integers between @${a} and @${b}, @${\sum_{i=a}^{b}\sqrt{i}}:


@def[
(define (square-roots-sum a b)
  (if (> a b)
    0
    (+ (sqrt a)
       (square-roots-sum (+ a 1) b))))
]

@incremental[(square-roots-sum 1 4)]

The mere observation of the functions' definition shows that they have
a common structure characterized by the following definition:

@lispcode[
(define (sum-??? a b)
  (if (> a b)
    0
    (+ (??? a) 
       (sum-??? (+ a 1) b))))
]

Now, this definition is no more than a sum of a mathematical
expression between two limits, i.e., a summation @${\sum_{i=a}^{b}f(i)}. The
summation is a mathematical abstraction for a sum of numbers described by a
mathematical expression relative to the summation index which ranges from
the lower to the upper limit. The mathematical expression is, therefore,
a function of the summation index.

The symbol @lisp[???] represents the mathematical expression to
perform inside the sum and that we will simply transform into a
parameter, by writing:


@def[
(define (summation f a b)
  (if (> a b)
    0
    (+ (f a)
       (summation f (+ a 1) b))))
]

We can now trivially evaluate different summations:

@def[
> (summation sqr 1 4)
30
> (summation sqrt 1 4)
6.14626
]

Because it accepts a function as an argument, the sum is a
higher-order function. The derivative of a function is another
example. The derivative @${f'} of a function @${f} is a function that
receives another function @${f} as argument and returns another
function---the derivative function of @${f}---as a result.

Generally, higher-order functions show up because there are two or more
functions with a similar structure. In fact, the repetition of a
pattern along two or more functions is a strong indicator of the need
for a higher-order function.

@section{Anonymous Functions}

The possibility of using higher-order functions opens up a huge 
range of new applications. For example, if we want to calculate the summations
@$${\sum_{i=1}^{10}i^2+3i} @$${\sum_{i=1}^{100}i^3+2i^2+5i} we need not
do more than to define the functions @${f_1(x)=x^2+3x} and
@${f_2(x)=x^3+2x^2+5x} and use them as argument of the @fn[summation]
function:

@def[
(define (f1 x)
  (+ (* x x) (* 3 x)))

(define (f2 x)
  (+ (* x x x) (* 2 x x) (* 5 x)))
]

@incremental[(summation f1 1 10)(summation f2 1 100)]

As can be seen, we can now easily calculate summations of different
functions. However, there is a negative aspect to note: if, for each
summation we intend on calculating, we have to define the function in
question, we will "pollute" our Racket environment with countless
definitions of functions of which their utility will be, at most, to serve as
argument to calculate the corresponding summation. Since each function must
be associated with a name, we will also have to come up with names for all
of them, which could be difficult, particularly, when we know that
these functions are useful for nothing else. In the last two examples
this problem was already visible: the names @fn[f1] and @fn[f2]
only reveal the difficulty in finding more expressive names.

In order to solve this problem, we should examine in greater detail
the concept of defining a function. So far we have said that, for
defining a function, we have to use a combination that begins with the
word @lisp[define], followed by a list in which the first element is the name
of the function to define and of which the remaining elements are the
function's parameters, followed by the function's body. For example, for
the function @${x^2=x\times x}, we write:

@def[
(define (sqr x) (* x x))
]

We have seen that after the previous definition, the @fn[sqr]
symbol becomes associated with a function:

@incremental[sqr]

However, we saw in section @secref{sec:globalVariables} that in order to define
constants we should use the @lisp[define] operator in a combination that
includes the name we want to define followed by the expression that we intend
to associate with that name. For example:

@def[
(define fi (/ (+ 1 (sqrt 5)) 2))
]

We also saw that after the previous definition, the @lisp[fi] symbol
becomes associated with a value:

@incremental[fi]

By analysing these examples we can conclude that the only difference
between a function definition and a definition of a constant comes
down to how the defined @emph{value} is obtained in one case and the
other. In the definition of constants, the value is the result of the
evaluation of an expression. In the definition of a function, the
value is a function that is constructed from the description of a list
of parameters and the body of a function. This leads us to think that
if it were possible to have an expression of which the evaluation
produced a function, then it would be possible to define functions as
if we were defining constants. For example, in the case of the
function @fn[sqr], if @${\lambda} is that expression, it should be
possible to write @lisp[(define sqr #,(math-in "\\lambda"))].

If the expression @lisp[(define sqr #,(math-in "\\lambda"))] associates to the
symbol @fn[sqr] the function created by the evaluation of the expression
@${\lambda}, what will be then the evaluation of the expression @${\lambda}?
Obviously, it has to be a function, but, as it is not yet associated with any
name, it is what is called as unnamed function or @emph{anonymous
function}. Note that an anonymous function is a function like any other, but
has the particularity of not being associated with any name.

We are left with knowing the form of @${\lambda} expressions. They
have a similar syntax to the definition of functions, but they omit
the function's name (logically) and replace the @lisp[define] symbol
with @ldots @lisp[lambda], i.e.: @footnote{The use we made of the
@${\lambda} symbol was not innocent. It derives from the origins of
the mathematical concept of anonymous function: @emph{@${\lambda}}
calculus, a mathematical model for @emph{computable} functions, i.e.,
functions whose call can be evaluated mechanically.}

@lispcode[
(lambda (#,(lispemphi parameter "1") ... #,(lispemphi parameter "n")) 
  #,(lispemphi expr "n+1")
  #,(lispemphi expr "n+2")
  ...
  #,(lispemphi expr "n+m"))
]
 
Any expression with the previous form, when evaluated, returns an
anonymous function. These expressions are designated, in Racket, as
@emph{lambda expressions}. Let us now notice the following:

@incremental[(lambda (x) (* x x))(define sqr (lambda (x) (* x x)))(sqr 3)]

As you can seen, the evaluation of the lambda expression returns
something of which the external representation indicates that a
procedure was created. When we associate the anonymous function with a
name, that name becomes the designation of the function, and can be
used as if it had been originally defined as a function. In practice,
the definition of functions using the @lisp[define] operator


@lispcode[
(define (#,(lispemph name) #,(lispemphi parameter "1") ... #,(lispemphi parameter "n"))
  #,(lispemphi expr "n+1")
  #,(lispemphi expr "n+2")
  ...
  #,(lispemphi expr "n+m"))
]

is equivalent to:

@lispcode[
(define #,(lispemph name) (lambda (#,(lispemphi parameter "1") ... #,(lispemphi parameter "n"))
  #,(lispemphi expr "n+1")
  #,(lispemphi expr "n+2")
  ...
  #,(lispemphi expr "n+m")))
]

This equivalence is easily tested with the redefinition of functions
that, now, can be defined as the creation of an association between a
name and an anonymous function. The previous example demonstrated this
possibility for the @fn[sqr] function but it exists for any other
function. For example, the function that calculates the area of a
circle can be defined by either:

@def[
(define (circle-area radius)
  (* pi (sqr radius)))
]

or by:

@def[
(define circle-area
  (lambda (radius)
    (* pi (sqr radius))))
]

Although from the syntactic point of view, the form 
@lisp[(define (f ...) ...)] 
is equivalent to @lisp[(define f (lambda (...) ...))], there is a
semantic difference with important implications: the first form does
not evaluate any of its arguments, whereas the latter form evaluates
its second argument. This allows the second form to define functions
with more sophisticated forms.


The use of anonymous functions has yet another advantage that becomes
evident when we analyse the @fn[function-points] function:

@def[
(define (function-points p f alpha beta gamma x0 x1 dx)
  (if (> x0 x1)
    (list)
    (cons (+xy p x0 (f alpha beta gamma x0))
          (function-points p f alpha beta gamma (+ x0 dx) x1 dx))))
]

As we have seen, in order to generate a list of points of, for example,
a sinusoid, we can invoke this function the following way:

@lispcode[
(function-points (xy 0 0)
                 sinusoid 0.75 0.5 0 
                 0 15 0.4)
]

Note that, for having been defined as a generalization of the
functions @fn[sinusoid-points] and @fn[parabola-points], the function
@fn[function-points], in addition to having introduced the @lisp[f]
function as a parameter, it also generalized the @lisp[a],
@lisp[omega] and @lisp[fi] parameters of the function
@fn[sinusoid-points] and @lisp[xv], @lisp[yv] and @lisp[d] of the
function @fn[parabola-points], calling them, respectively,
@lisp[alpha], @lisp[beta] and @lisp[gamma]. It so happens that these
parameters never change during the recursive calls performed by the
@fn[function-points] function. In fact, the function passes these
parameters from recursive call to recursive call only because they are
necessary for the invocation of the @lisp[f] function. Let us now
imagine that, instead of passing these parameters, they were
associated to the @lisp[f] function itself that we had just passed. In
this case, we could rewrite the @fn[function-points] function in the
following form:


@def[
(define (function-points p f x0 x1 dx)
  (if (> x0 x1)
    (list)
    (cons (+xy p x0 (f x0))
          (function-points p f (+ x0 dx) x1 dx))))
]

With this new definition, the previous call would have to be rewritten
as:

@lispcode[
(function-points (xy 0 0)
                 (lambda (x) (sinusoid 0.75 0.5 0 x)) 
                 0 15 0.4)
]

Although it may not seem to be a substantial gain, rewriting the
function @fn[function-points] has a considerable advantage: it allows
the use of any base functions, regardless of the number of parameters
it has: if they have only one parameter, they can be used directly,
otherwise we can "wrap" them with an anonymous function of one
parameter only that invokes the desired function with all the required
arguments. This makes the function @fn[function-points] even more
generic, as it is visible in following examples:

@incremental[
(spline
  (function-points (xy 0 6)
                   sin
		   0 4pi 0.2))

(spline
  (function-points (xy 0 3)
		   (lambda (x) 
                     (- (expt (/ x 10) 3)))
		   0 4pi 0.5))

(spline
  (function-points (xy 0 0)
		   (lambda (x)
                     (damped-oscillatory 1.5 0.4 4 x))
		   0 4pi 0.05))
]

which create the curves shown in @figref{fig:curvasLambda}.

@figure[#:tag "fig:curvasLambda"
        #:caption @elem{Curve generated by the function @fn[function-points]. From top to bottom, we have a sinusoid, an inverted exponential and a damped sinusoid.}]{
@(show-tikz 1.2)
}

Even though the generalization of higher-order functions allows these
to be used in a wide variety of situations, sometimes, it is
preferable to encapsulate a particular use in a function more easily
recognizable. For example, if a program is systematically computing
points of a sinusoid, then it is likely that this program becomes more
readable if in fact the @fn[sinusoid-points] function exists. But even
in this case, the order functions will allow this function to be
defined in a simpler way. That way, instead of having to write the
usual recursive definition:

@def[
(define (sinusoid-points p a omega fi x0 x1 dx)
  (if (> x0 x1)
    (list)
    (cons (+xy p x0 (sinusoid a omega fi x0))
          (sinusoid-points p a omega fi (+ x0 dx) x1 dx))))
]

we can now write:

@def[
(define (sinusoid-points p a omega fi x0 x1 dx)
  (function-points
    p
    (lambda (x) (sinusoid a omega fi x))
    x0 x1 dx))
]


@questions[
@question{
Consider the balconies shown on the following image:

@fig[@autoimage{varandasOrdemSuperior}]

The balconies are composed of a slab and a guardrail, the guardrail being
composed by the uprights and the handrail. Define a function called
@fn[balcony] which, conveniently parametrized, is capable of generating not
only the balconies shown in the previous image but also many
others. For that, the @fn[balcony] function should receive not only
the geometric parameters of the balcony (as is the slab's thickness
or the height of the handrail, etc.), but also the function which
determines the outside curve of the balcony.  }

@question{ Define the necessary functions and write the Racket expressions
  that reproduce the balconies shown in the previous image.  }


@question{ Consider the following image where we present a sequence of
  cylindrical tubes joined by spheres that make up a random path in
  which the path sections are parallel to the coordinate
  axes. Note that the tubes have a random length between a maximum
  length and @${10\%} of that maximum length and that the
  changes in direction are also random but never the reverse of the
  immediately previous direction. Also note that the tubes have a
  radius that is @${2\%} of the maximum length of the tube and that
  the spheres have a radius that is @${4\%} of the maximum length of
  the tube.

@fig{@autoimage[#:scale 0.4]{tubosAleatorios}}

  Define the function @fn[path-of-tubes] that, given the point and the
  initial direction, the maximum length of tube and the number of
  tubes, creates a sequence of tubes joined by spheres that follow a
  random path.  }

@question{ Even though the path can be random, it is
  possible to limit it in space so as to never exceed a certain
  region. This limitation is visible in the following image where,
  from left to right, we can see a sequence of tubes following a
  random path limited by a cube, a random path limited by a cylinder,
  and finally, a random path limited to a sphere.

  @fig{@autoimage[#:scale 0.36]{tubosCubo}
    @autoimage[#:scale 0.28]{tubosCilindro}
    @autoimage[#:scale 0.28]{tubosEsfera}}

  Define the function @fn[tubes-shape] as a generalization of the
  function @fn[path-of-tubes] in order to receive, in addition to
  the parameters of the latter, an additional parameter that should be
  a predicate which, given a hypothetical point to the path extension,
  indicates whether this point is contained in the region in question. If
  it is not, the program should reject this point and generate a new
  one.

  Also define the functions @fn[cube-of-tubes],
  @fn[cylinder-of-tubes] and @fn[sphere-of-tubes], which have
  the same parameters of the function @fn[path-of-tubes] and that
  use the function @fn[path-of-tubes] with the appropriate
  predicate to the desired shape.  }
]

@section{Identity Function}

We saw that the @fn[summation] function implements the classic summation used
in algebra @${\sum_{i=a}^bf(i)}:

@def[
(define (summation f a b)
  (if (> a b)
    0
    (+ (f a)
       (summation f (+ a 1) b))))
]


All summations can be calculated simply by specifying, on the one hand, the
function @${f} which provides each term of the sum and, on the other, the
@${a} and @${b} limits of that summation. For example, the mathematical
expression @${\sum_{i=1}^{10}\sqrt{i}} is calculated by the
corresponding Racket expression:

@lispcode[
(sum sqrt 1 10)
]

In case the summation terms are calculated by an expression for which
Racket does not have a pre-defined function, the more immediate
solution will be to use a lambda expression. For example, to calculate
@$${\sum_{i=1}^{10}\frac{\sin i}{\sqrt{i}}}

we can write:

@lispcode[
(summation (lambda (i) (/ (sin i) (sqrt i))) 1 10)
]

In general, the specification of the function @${f}, which computes
each term of the summation, does not raise any difficulties but there is a
particular case which may cause some perplexity and therefore deserves
being discussed. Let us consider the @${\sum_{i=1}^{10}i} summation. Which
is the Racket expression that calculates it?

In this latter example, it is not entirely clear what is the function
@${f} that is at stake in the calculation of the summation, because the
observation of the mathematical expression correspondent to the term
of the summation does not allow the uncovering of any function. And yet, it is
there. To make it clearer we have to remember that the
@fn[summation] function calculates @${\sum_{i=a}^bf(i)}, whereby, in this
latter example, the parameters in question will have to be @${a=1},
@${b=10} and, finally, @${f(i)=i}. It is this last function that is
relatively strange: given an argument, it merely returns this
argument, without performing any operation on it. Mathematically
speaking, this function is called @emph{identity function} and
corresponds to the neutral element of the composition of functions. The
identity function can be trivially defined in Racket based on its
mathematical definition:


@def[
(define (identity x) x)
]

Although it seems to be a useless function, the @lispf{identity
function} function is in reality, very useful. First of all, because it
allows us to calculate the @${\sum_{i=1}^{10}i} summation just by writing:


@lispcode[
> (sum identity 1 10)
55
]

Many other problems also benefit from the identity function. For
example, if we define the function that computes the product
@${\prod}:

@$${\prod_{i=a}^{b}f(i)=f(a)\cdot f(a+1)\cdot\cdots\cdot f(b-1)\cdot f(b)}

@def[
(define (product f a b)
  (if (> a b)
    1
    (* (f a) 
       (product f (+ a 1) b))))
]

it becomes trivial to define the factorial function by the
product: @$${n!=\prod_{i=1}^{n}i}

@def[
(define (factorial n)
  (product identity 1 n))
]

Further ahead we will find new uses for the identity function.


@questions[ @question{ Both the summation and the product can be seen as
special cases of another even more generic abstraction, designated
accumulation. In this abstraction, the parameters are: the operation of
combination of elements, the function to apply to each one, the initial value,
the lower limit, the transition to the next element (designated as successor)
and the upper limit. Define this function. Also define the summation and the
product in terms of accumulation.  }


@question{ It is known that the sum @${\frac{8}{1\cdot
  3}+\frac{8}{5\cdot7}+\frac{8}{9\cdot11}+\cdots} converges (very slowly) to
  @${\pi}. Using the @fn[accumulation] function defined in previous exercise,
  define the function that calculates an approximation of @${\pi} to the
  @${n}-th term of the sum. Determine an approximation of @${\pi} to the term
  @${2000}.  }

@question{ The @fn[enumerate] function is capable of generating sequences
  in arithmetic progression, i.e. sequences in which there is a constant
  difference between each two elements. For example,
 
@incremental[(enumerate 0 20 2)]

It may be necessary, however, to produce sequences in which the
elements develop differently, for example, in geometric progression.

Define the higher-order function @fn[succession] that receives the
limits @${a} and @${b} of a range and also the function @${f} and
generates a list with all the elements @${x_i} that do not exceed
@${b} such that @${x_0=a} and @${x_{i+i}=f(x_i)}. For example:


@lispcode[
> (succession 2 600 (lambda (x) (* x 2)))
(2 4 8 16 32 64 128 256 512)
]
}

@question{ Redefine the @fn[enumerate] function in terms of the
  @fn[succession] function.  }
]

@section{The Function Restriction}

In previous we saw several examples of High-Order functions that
received functions as arguments. In this section we will look at
High-Order functions that produce other functions as a result.

Let us start by considering a function that computes the double of a
given number:

@def[
(define (double x)
  (* 2 x))
]

We can immediately see that the function @fn[double] represents a
particular case of multiplication between two numbers. More
specifically we say that the function @fn[double] is a
@emph{restriction} of the function @fn[*] where the first operand is
always 2. The same can be said for the functions that calculate the
triple or quadruple of a number.

As a second example let us consider the exponential function @fn[e^x],
where @${e^x = 2.718281828459045} is the base of Neperian
logarithm. This function can be defined in terms of the Exponentiation
function @fn[a^b]:

@def[
(define (exponential x)
  (expt 2.718281828459045 x))
]

As we can see the function @fn[exponential] uses the function
@fn[expt] systematically with the same operand. Once more, it is a
@emph{restriction} of a function that has a fixed operand.

This type of restriction, where an operation is used between two
operands and always with the same first operand, is a pattern that can
be easily implemented using a High-Order function.  To better
understand the definition of this function let us first write the two
previous functions using anonymous functions. As we saw in section 0.4
every definition of a function implies the creation of an anonymous
function that is then associated to a given name. We have:

@def[
(define double
  (lambda (x)
	(* 2 x)))
]

@def[
(define exponential
  (lambda (x)
	(expt 2.718281828459045 x)))
]

By comparing these two anonymous functions we can tell that the only
difference between them is simply the function used (@fn[*] in on case
and @${e^x = 2.718281828459045} in the other) and the first operand
(@${2} in one case and @${2.718281828459045} in the other). This
suggests that we can define a function that receives these two
differences as parameters and produces the corresponding anonymous
function:

@def[
(define (restriction f a)
  (lambda (x)
    (f a x)))
]

Using this function we can now write:

@def[
(define double (restriction * 2))
]

@def[
(define exponential (restriction expt 2.718281828459045))
]

and use them as any other function:

@lispcode[
> (double 3)
6
> (exponential 2)
7.3890560989306495
]

The most interesting aspect of the function @fn[restriction] is that it
produces a function as a result. That allows it to be used not only to
define other functions but to let us write more simple expressions as
well. For example, in Mathematics we define the n-th harmonic number
with the formula:

@${H_n=1+\frac{1}{2}+\frac{1}{3}+\cdots\frac{1}{n}={\sum_{k=1}^{n}\frac{1}{k}}}

Using the function @fn[restriction] we can define the previous function in Racket:

@def[
(define (harmonic-number n)
  (summation (restriction / 1) 1 n))
]

Naturally, if the expression @fn[(restriction / 1)] is to be
repeatedly used then it is convenient to give it a name:

@def[
(define inverse (restriction / 1))
]

so that we can simply write:

@def[
(define (harmonic-number n)
  (summation inverse 1 n))
]

The function @fn[restriction] is useful enough for it to be
pre-defined in Racket under the name @fn[curry], in honour of
Mathematician Haskell Curry. @footnote{Haskell Curry was a 20th
century American Mathematician who made important contributions in the
field of Combinatory Logic, collaborated in developing one of the
first computers and invented one of the first programming languages.}

@questions[
@question{Similar to what we did to the functions @fn[double],@fn[exponential], and @fn[inverse],
use the function @fn[restriction] (or @fn[curry]) to define the functions @fn[successor] and
@fn[symmetrical] capable of calculating the successor of a number and the symmetrical value
of a number, respectively.}

@question{The function @fn[curry] is also useful for defining predicates. Using this function
define the predicate @fn[negative?], that returns true for all negative numbers, and the predicate
@fn[one?], that returns true only for the number 1.}
]

Even though the function @fn[curry] allows for simplifying the
creation of restrictions of functions, it only solves half the
problem. For us to see the other half of the problem we need only
think of functions such as the square of a number or the predecessor
of a number. Their definitions are:

@def[
(define (square-number x) (expt x 2))
]

@def[
(define (predecessor n)	(- n 1))
]

In these two examples we notice that they too are restrictions of
other existing functions but this time with a fixed second
operand. This prevents the use of @fn[curry] since it produces
functions with a fixed first operand, but nothing prevents us from
defining an alternative form of restriction that applies to the second
operand. In fact, this second alternative of restriction is already
pre-defined in Racket under the name @fn[curryr], where @fn[r] means
@fn[right] to indicate the function fixes the operand on the
right. Using this High-Order function we get:

@def[
(define square-number (curryr expt 2))
]

@def[
(define predecessor (curryr - 1))
]

@questions[
@question{Define the function @fn[curryr].}
]


@section{The Composition Function}

One of the most useful High-Order functions is the one which allows
the @emph{composition} of functions.  Given two functions @emph{f} and
@emph{g}, the composition of @emph{f} with @emph{g} is written @${(f
\circ g\)} and is defined by the function:

@${(f \circ g)(t)=f(g(t))}

This way of defining the composition of functions shows that the
function @emph{f} is to be applied to the result of @emph{g} but what
it does not show is that it is in fact defining the function
@${\circ}. To make this definition more apparent it is useful to use
the much more formal notation @${\circ(f,g)}. In this form it becomes
much more evident that @${\circ} is a function that takes other
functions as arguments. With just that we can state that @${\circ} is
a high order function but it actually does more than just receiving
two functions as arguments, it also produces a new function as a
result that, for a given parameter @emph{t}, calculates
@${f(g(t))}. Seeing as this new function has no name the best way of
producing it is with a @fn[lambda] expression. The correct definition
of the combination function is:

@${\circ(f,g)=\lambda tf(g(t))}

We can now define it in Racket:

@def[
(define (composition f g)
  (lambda (t)
    (f (g t))))
]

Using the function @fn[composition] we can now create other functions
more easily. For example, we know that the number
@${e=2.718281828459045}, the base of Neperian numbers, can be defined
in terms of the function summation:

@${\sum_{n=0}^{\infty}\frac{1}{n!}}

Since the summation function uses the composition of the functions
@fn[inverse] and @fn[factorial] we can get close value of @emph{e} by
computing only a few terms of the summation:

@incremental[(summation (composition inverse factorial) 0.0 20.0)]

Just as we did in the previous section it might also be useful here to
not only create but also to define functions in terms of the
composition of other functions. For example, if we wish to have a
function that gives us the second element of a list we know that we
must first use the function @fn[cdr], to obtain the list without the
first element, and then the function @fn[car], to get the first
element of the resulting list. This is nothing less than the
composition of the function @fn[car] and @fn[cdr]:

@def[
(define second-element (composition car cdr))
]

Since the result of @fn[composition] is a function, the name
@fn[second-element] designates that function and should be used as
such:

@incremental[(second-element '(a b c))]

Likewise, if we wish to define a function that gives us the third
element of a list we can write:

@def[
(define third-element (composition car (composition cdr cdr)))
]

Obviously for arbitrary combinations of functions it might me useful
to use a more compact way of writing them. For that we can group the
functions we wish to compose in a list, and write:

@def/no-show[
(define (compositions fs)
  (if (null? fs)
    identity
    (composition (car fs)
    		 (compositions (cdr fs)))))
]

@lispcode[
(define fourth-element (compositions (list car cdr cdr cdr)))
]

Since we need to process a list of functions we can define the
function @fn[compositions] using recursion, where we make the
composition of the first element of the list with the result of
composing the remaining elements- Here is a first sketch of this
function:

@def[
(define (compositions fs)
  (if (null? fs)
      ???
      (composition (car fs)
                   (compositions (cdr fs)))))
]

What we need to find out is what should come in place of @fn[???]. If
we collapse the computational process for @lisp[(compositions (list f g h))], we have:

@lispcode[
(composition f (composition g (composition h ???)))
]

It is logical to thing that instead of @fn[(composition h ???)]
we should have just @fn[h] which means that instead of @fn[???] we
should have the @emph{neutral element} of the composition of
functions. That element is none other than the @emph{identity}
function, for which we will now have:

@def[
(define (compositions fs)
  (if (null? fs)
    identity
    (composition (car fs)
    		 (compositions (cdr fs)))))
]

Given the usefulness of the @fn[composition] function it is already
pre-defined in Racket under the name of @fn[compose], which takes any
number of arguments and returns as result their composition.

@questions[
@question{Define the function @fn[power-of-four] in terms of the functions @fn[power-of-two] and @fn[compose].}

@question{Define the function @fn[identity] in terms of the function @fn[compose].}

@question{Using the functions @fn[curry] and @fn[compose], define the
function @fn[symmetrical-function] that, given the function @${f(x)}
produces the symmetrical function @${-f(x)}. Its evaluation must
produce the following interaction:

@def/no-show[
(define (symmetrical-function f)
  (compose (curry - 0) f))
]

@incremental[(define -sqrt (symmetrical-function sqrt))(-sqrt 2)]
}
]


@section{Higher Order Functions on Lists}

We have seen, upon the introduction of lists as recursive data
structure, that the processing of lists was easily accomplished
through the definition of recursive functions. In fact, the behaviour
of these functions was quite stereotypical: the functions began by
testing whether the list was empty, and if not, the first element of
the list would be processed, processing the remaining by a recursive
invocation.

As we have seen in the previous section, when some functions have a
similar behaviour, it is advantageous to abstract them in a
higher-order function. That is precisely what we are going to do now
by defining higher-order functions for three common cases of list
processing: mapping, filtering and the reduction.


@subsection{Mapping}

One of the most useful operations is the one that transforms a list
into another list by applying a function to each element of the first
list. For example, given a list of numbers, we might be interested in
producing another list containing the squares of these numbers. In
this case we say that we are @emph{mapping} the square function onto a
list to produce the list of squares. The function definition presents the
typical pattern of recursion on lists:

@def[
(define (mapping-square lst)
  (if (null lst)
    (list)
    (cons (sqr (car lst))
          (mapping-square (cdr lst)))))
]

Obviously, defining a function to only map the square is to
particularize in excess. It would be much more useful to define a
higher-order function that maps any function on a list. Luckily, it is
trivial to modify the previous function:

@def[
(define (mapping f lst)
  (if (null? lst)
    (list)
    (cons (f (car lst))
          (mapping f (cdr lst)))))
]

With this function it is trivial to produce, for example, the square
of all the numbers from @${10} to @${20}:

@incremental[(mapping sqr (enumerate 10 20 1))]

The function @fn[mapping] already exists pre-defined in Racket with
the name @fn[map]. The implementation provided in Racket also
allows mapping over multiple lists simultaneously. For example, if we
want to sum the elements of two lists two by two, we can write:


@incremental[(map + (enumerate 1 5 1) (enumerate 2 6 1))]

In addition to this function, Racket also provides a syntactic variant
that can be useful to avoid writing anonymous functions: the form
@lisp[for/list]. Using this form, the above calculation can be carried
out by:


@incremental[
(for/list ((x (enumerate 1 5 1))
           (y (enumerate 2 6 1)))
     (+ x y))
]

@subsection{Filtering}

Another useful function is the one that @emph{filters} a list. The
filtering is performed by providing a predicate that is applied to each
element of the list. The elements which satisfy the predicate (i.e., for
which the predicate is true) are collected in a new list.

The definition is simple: 

@def[
(define (filtering p lst)
  (cond ((null? lst)
         (list))
        ((p (car lst))
         (cons (car lst)
               (filtering p (cdr lst))))
        (else
         (filtering p (cdr lst)))))
]

Using this function we can, for example, get only the squares
emph{divisible by @${3}} of the numbers between @${10} and @${20}:

@incremental[
(filtering (lambda (n) (= (remainder n 3) 0)) 
           (mapping sqr (enumerate 10 20 1)))
]

The function @fn[filtering] already exists pre-defined in Racket with
the name @fn[filter].

@subsection{Reduction}

A third function that is very useful is the one that performs a
@emph{reduction} in a list. This higher-order function receives an
operation, an initial element and a list and will reduce the list
through the "interleaving" operation between all elements of the
list. For example, to add all the elements of a list
@lispemph[l]@${=}@lisp[(#,(lispemphi e "0") #,(lispemphi e "1") ...
  #,(lispemphi e "n"))] we can do @lisp[(reduce + 0 #,(lispemph l))] and
obtain
@lispemphi[e]{0}@${+}@lispemphi[e]{1}@${+}@ldots @${+}@lispemphi[e]{n}@${+}0.
Thus, we have:

@def/no-show[
(define (reduce f v lst)
  (if (null? lst)
    v
    (f (car lst)
       (reduce f v (cdr lst)))))
]

@incremental[
(reduce + 0 (enumerate 1 100 1))
]

The function definition is quite simple:

@lispcode[
(define (reduce f v lst)
  (if (null? lst)
    v
    (f (car lst)
       (reduce f v (cdr lst)))))
]

To see a more interesting example of the use of this functions, let us
consider calculating the factorial of a number. According to the, non-recursive,
traditional definition we have:

@$${n! = 1\times 2\times 3\times \cdots{} \times n}

Differently put, it is the product of all the numbers of an
enumeration from 1 to n, i.e.:

@def[
(define (factorial n)
  (reduce * 1 (enumerate 1 n 1)))
]

As we can confirm in the previous examples, the initial value used is
the neutral element of the combination operation of the list elements,
but it need not be necessarily so. To see an example
where this does not happen, let us consider determining the
highest value existing in a list of numbers. In this case, the
function that we use to successively combine the values of the list is
the @fn[max] function, which returns the greatest of two numbers. If
@lispemph[l] is the list of numbers, @lisp[(#,(lispemphi e "0")
  #,(lispemphi e "1") #,(lispemphi e "2") ...)], the greater of those
numbers can be obtained by @lisp[(max #,(lispemphi e "0") (max
  #,(lispemphi e "1") (max #,(lispemphi e "2") ...)))] which,
obviously, corresponds to a list reduction using a @fn[max]
function. We are left with determining the initial element to be
used. One hypothesis is to use the negative infinity @${-\infty}
since any number will be greater than it. Another, more simple, will be
to use any of the elements of the list, particularly the first as it
is the one with easiest access. Thus, we can define:
  

@def[
(define (max-list lst)
  (reduce max (car lst) (cdr lst)))
]


The function @fn[reduce] already exists pre-defined in Racket with
the name @fn[foldr]
@lisp[(foldr #,(lispemph f) #,(lispemph i) '(#,(lispemphi e "0") #,(lispemphi e "1") #,(lispemphi e "2") ...
  #,(lispemphi e "n")))] calculates @lisp[(#,(lispemph f) #,(lispemphi e "0")
  (#,(lispemph f) #,(lispemphi e "1") (... (#,(lispemph f) #,(lispemphi e "n")
  #,(lispemph i)))))]

There is another similar function called
@fn[foldl] that performs the combination of elements in another
order: @lisp[(foldl #,(lispemph f) #,(lispemph i) '(#,(lispemphi e "0")
  #,(lispemphi e "1") #,(lispemphi e "2") ... #,(lispemphi e "n")))]
calculates @lisp[(#,(lispemph f) (... (#,(lispemph f) (#,(lispemph f)
  #,(lispemphi e "0") #,(lispemph i)) #,(lispemphi e "1")) ...)
  #,(lispemphi e "n"))]

This order of difference allows the function @fn[foldl] to compute the
result in a more efficient manner than is possible with the @fn[foldr]
function. However, one must keep into account that applying the
the function @fn[foldl] is only equivalent to the function
@fn[foldr] when the combination function @lispemph[f] is
commutative and associative.

@questions[
@question{Consider a surface represented by a set of
  three-dimensional coordinates, as presented in the following figure:

@fig{
@tex{
\NEisometric{}
\begin{tikzpicture}[tdplot_main_coords]
  % \draw (0,0) -- (10,0)node{$x$};
  % \draw (0,0) -- (0,10,0)node{$y$};
  % \draw (0,0) -- (0,0,1)node{$z$};
  \footnotesize
  \foreach \i in {0,...,5}
   \foreach \j in {0,...,5}
     \filldraw (\i,\j,{0.3*(sin(\i r)+sin(2*(\j-2) r))}) circle(0.3mm)node[above]{$p_{\i,\j}$};
\end{tikzpicture}
}}

Assume that these coordinates are stored in a list of lists in the
form:

@tex{
\newcommand{\foo}[2]{\(#1_{#2}\)}
((\foo{p}{0,0} \foo{p}{0,1} ... \foo{p}{0,5})
 (\foo{p}{1,0} \foo{p}{1,1} ... \foo{p}{1,5})
 ...
 (\foo{p}{5,0} \foo{p}{5,1} ... \foo{p}{5,5}))
}

Define the function @fn[surface-interpolation] that receives a list
with the previous form, starts by creating a list of @emph{splines}
in which each @emph{spline} @${S_i} passes through the points
@${(p_{i,0}, p_{i,1},\ldots,p_{i,5})} as shown in the following image on
the left and ends up using this list of splines
@${S_0,S_1,\ldots,S_5} to make a smooth interpolation of sections, as
shown in following image on the right:


@fig{
@tex{
\NEisometric
\begin{tikzpicture}[tdplot_main_coords,scale=0.65]
  \footnotesize
  % \draw (0,0) -- (1,0);
  % \draw (0,0) -- (0,1,0);
  % \draw (0,0) -- (0,0,1);
  \foreach \i in {0,...,5}
   \foreach \j in {0,...,5}
     \filldraw (\i,\j,{0.3*(sin(\i r)+sin(2*(\j-2) r))}) circle(0.3mm)node[above]{$p_{\i,\j}$};
  \pgfplothandlercurveto
  \foreach \i in {0,...,5} {
    \pgfplotstreamstart
    \foreach \j in {0,...,5} {
      \pgfplotstreampoint{\pgfpointxyz{\i}{\j}{0.3*(sin(\i r)+sin(2*(\j-2) r))}}
    }
    \pgfplotstreamend
    \pgfusepath{stroke}
  }
  \end{tikzpicture}
}
@autoimage[#:scale 0.44]{superficie}
}}]

@section{Generation of Three-Dimensional Models}

Until now, we have used the CAD tool just as a visualizer of the forms that
our programs generate. We will now see that a CAD tool is not only a
drawing program. It is also a database of geometric figures. In fact,
every time we draw something the CAD tool records
the created graphic entity in its database, as well as some additional information
related to that entity, for example its colour.

There are several ways to access the created entities. One of the
simplest ways for a "normal" CAD tool user will be by using the mouse,
simply by "clicking" on the graphic entity to which we want to
access. Another way, more useful for those who want to program, will
be by calling Racket's functions that return, as results, the existing
geometrical entities. There are several of those functions at our disposal but,
for now, let us confine ourselves to one of the simplest: the function
@fn[all-shapes]

The @fn[all-shapes] function does not receive any arguments and
returns a list of all existing geometric entities in the CAD tool. Naturally,
if there are no entities, the function returns an empty list.

The following interaction demonstrates the behaviour of this function:

@verbatim{
> (all-shapes)
'()
> (circle (xy 1 2) 3)
#<circle 0>
> (sphere (xyz 1 2 3) 4)
#<sphere 1>
> (all-shapes)
'(#<circle 2> #<solid 3>)
}

In the previous interaction we notice that the function
@fn[all-shapes] creates representations of the geometric entities
existent in the CAD tool without having any idea of how these entities
got there, which prevents it from relating the existing
forms, such as @verb{#<circle 2>}, with forms that were created from
Rosetta, such as @verb{#<circle 0>}. Furthermore, the CAD tool does not always
provide information about the type of geometrical form in question,
which explains the fact that the sphere @verb{#<sphere 1>} created by
Rosetta will subsequently be recognized just as the solid
@verb{#<solid 3>}.

When necessary (and possible), these forms can be identified by the
recognizers @fn[point?], @fn[circle?], @fn[line?],
@fn[closed-line?], @fn[spline?], @fn[closed-spline?],
@fn[surface?], and @fn[solid?].

Given a geometric entity, we may be interested in knowing its
@\emph{properties}. The access to these properties depends much on
what the CAD tool is able to provide. For example, for a circle, we
can know its centre through the function @fn[circle-center] and its
radius through the function @fn[circle-radius], while for a point
the function @fn[point-position] returns its position and for a
polygonal line, the function @fn[line-vertices] produces a list
with the positions of the vertices of that line. But for a generic
solid, we have no access to any property.

In summary, we have:

@lisp[(point-position (point _p)) = _p]

@lisp[(circle-center (circle _p _r)) = _p]

@lisp[(circle-radius (circle _p _r)) = _r]

@lisp[(line-vertices (line #,(lispemphi p "0") #,(lispemphi p "1") ... #,(lispemphi p "n")))] @${=} @lisp['(#,(lispemphi p "0") #,(lispemphi p "1") ... #,(lispemphi p "n"))]

While the above equivalences show the relations between the constructors
of geometric entities (@fn[point], @fn[circle],etc.) and the
selectors of these entities
(@fn[point-position],@fn[circle-center], etc.) it is important
to take into account that it is possible to use the selectors with
entities that were not created from Rosetta, but directly in the CAD
tool. This can be particularly useful when we want to write programs
that, instead of generating geometry, only process existing geometry.

We will now exemplify the use of these operations in solving a real
problem: the creation of three-dimensional models of buildings from
two-dimensional plans provided by municipal services. These
plans are characterized by representing each building with a
polygon that delimits it, containing, in its interior, a point that
indicates the building's height.  @Figref{fig:plantaCamara0} shows
a fragment of one of these plans, where a large enough icon was used
for the points in order to make them more visible.

@figure[#:tag "fig:plantaCamara0"
        #:caption @elem{Plan supplied by town hall services. Each building is represented by its surrounding polygon and a point at the building's height. Due to errors in plans, some buildings may not have their respective 
                        height while others may have more than one point. There may also be heights that are not associated with any building.}]{
@tex{\begin{tikzpicture}[scale=0.6]
\inputtikz{AmostraRato}
\end{tikzpicture}}}

Unfortunately, it is not uncommon to find plans with errors and the
example shown in @figref{fig:plantaCamara0} illustrates precisely
this: a careful observation will reveal buildings that do not
possess any height point, as well as height points
that are not associated with any building. Furthermore, it is possible
to find buildings with more than one height point. These are problems
that we have to take into account when we think about creating
programs that manipulate these plans.

In order to create a three-dimensional model from one of these plans
we can, for each polygon, create a region that we will extrude up to
the height indicated by the corresponding point. To do so, we
have to be able to identify, for each polygon, which point that is (or
what points those are in case of there being more than one).

Detecting if a point is contained within a polygon is a classic
problem of Computational Geometry for which there are various
solutions. One of the simplest is to draw a ray from this point and
count the number of intersections that this ray has with the edges
of the polygon, as illustrated in @figref{fig:pontoPoligono} with
several points. If the number of intersections is zero, then the
point is of course outside the polygon. If the number is one, then the
point is necessarily inside the polygon. If this number is two, then
it is because the radius entered and exited the polygon and, therefore,
the point is outside the polygon; if the number is three, then it is
because the radius exited, entered, and exited back out of the polygon
and, therefore, the point is inside the polygon. Since each radius
entry in the polygon implies its exit, it becomes evident that if the
number of intersections of the radius with the polygon is even, then
it is because the point was outside the polygon and if the number of
intersections is odd then it is because the point was inside the
polygon.

@figure[#:tag "fig:pontoPoligono"
        #:caption @elem{The use of a ray to determine if a point is contained within a polygon.}]{
@tex{
  \begin{tikzpicture}
    \inputtikz{pontoNoPoligono}
  \end{tikzpicture}
}}

The implementation of this algorithm is relatively trivial: given a
point @${P=(P_x,P_y)} and a list @${V_s} with the vertices of the
polygon @${V_0,V_1,\ldots{},V_n}, we "create" a ray with its origin in
@${P} and of which the destination is a point @${Q=(Q_x,Q_y)} sufficiently
away from the polygon. To simplify, we will arbitrate that this
ray is horizontal, which allows the destination point @${Q}
to have the same ordinate @${P}, i.e., @${Q_y=P_y}. To ensure that @${Q}
is sufficiently far enough from the polygon, we can calculate the largest abscissa
of the vertices of the polygon
@${\max({V_0}_x,{V_1}_x,\ldots,{V_n}_x)} and we add a small distance,
for example, one unit.

The calculation of the vertices greater abscissa is trivial to perform
when using higher-order functions: we can start by mapping the list of
vertices onto the list of its abscissa, and then, we operate a reduction
of this list with the @fn[max] function. This reasoning can be
translated into Racket by the following function:

@lispcode[
(define (point-in-polygon? p vs)
  (let ((q (xy (+ (foldl max (cx (car vs)) (map cx vs)) 1)
               (cy p))))
  ...))
]

Next, we determine the intersections between the segment defined by
points @${P-Q} and the segments that make up the polygon. Those
segments are defined by the vertices @${V_0-V_1}, @${V_1-V_2},
@ldots, @${V_{n-1}-V_n} and @${V_n-V_0}, that we can get through a
mapping along two lists, one with the points @${V_0,V_1,\ldots{},V_n}
and the other with the points @${V_1,\ldots{},V_n,V_0}. Our function
takes then the form:

@lispcode[
(define (point-in-polygon? p vs)
  (let ((q (xy (+ (foldl max (cx (car vs)) (map cx vs)) 1)
               (cy p))))
  ...(map (lambda (vi vj)
            ...)
          vs
          (append (cdr vs) (list (car vs))))))
]

The function that we map along the two lists of vertices should
produce, for each two consecutive vertices @${V_i} and @${V_j} the
intersection of the segment @${P-Q} with the segment @${V_i-V_j}.

To determine the intersection of two line segments, we can think as
follows: given the points @${P_0} and @${P_1} that delimit a line,
every point @${P_u} between @${P_0} and @${P_1} is determined by the
equation 
@$${P_u = P_0 + u(P_1 - P_0), 0\leq u\leq 1}

If this line
segment intersects another line segment delimited by points @${P_2}
and @${P_3} and defined by @$${P_v = P_2 + v(P_3 - P_2), 0\leq v\leq 1}
then it is clear that exists a @${u} and @${v} such as @$${P_0 + u(P_1 -
P_0) = P_2 + v(P_3 - P_2)}.


In the two-dimensional case, we have that @${P_i=(x_i,y_i)} and the
previous equation unfolds in the two equations:

@${x_0 + u(x_1 - x_0) = x_2 + v(x_3 - x_2)}
@${y_0 + u(y_1 - y_0) = y_2 + v(y_3 - y_2)}

Solving the system, we obtain

@$${u=\frac{(x_3-x_2)(y_0-y_2)-(y_3-y_2)(x_0-x_2)}{(y_3-y_2)(x_1-x_0)-(x_3-x_2)(y_1-y_0)}}
@$${v=\frac{(x_1-x_0)(y_0-y_2)-(y_1-y_0)(x_0-x_2)}{(y_3-y_2)(x_1-x_0)-(x_3-x_2)(y_1-y_0)}}

It is clear that for the divisions to be performed it is required that
the denominator @${(y_3-y_2)(x_1-x_0)-(x_3-x_2)(y_1-y_0)} is different
from zero. If that is not the case, it is because the lines are parallel. If they are
not parallel, the case of the intersection being outside 
the line segments in question can occur, and so we have to check if the
obtained values ​​@${u} and @${v} obey the conditions @${0\leq u\leq 1}
and @${0\leq v\leq 1}. When this happens, the exact point of
intersection can be calculated by replacing @${u} in the equation
@${P_u = P_0 + u(P_1 - P_0)}.

This leads us to the following definition for a function that
calculates the intersection:


@def[
(define (intersection-segments p0 p1 p2 p3)
  (let ((x0 (cx p0)) (x1 (cx p1)) (x2 (cx p2)) (x3 (cx p3))
        (y0 (cy p0)) (y1 (cy p1)) (y2 (cy p2)) (y3 (cy p3)))
    (let ((denominator (- (* (- y3 y2) (- x1 x0))
                          (* (- x3 x2) (- y1 y0)))))
      (and (not (zero? denominator)) ;;Parallel segments
           (let ((u (/ (- (* (- x3 x2) (- y0 y2))
                          (* (- y3 y2) (- x0 x2)))
                       denominator))
                 (v (/ (- (* (- x1 x0) (- y0 y2))
                          (* (- y1 y0) (- x0 x2)))
                       denominator)))
             (and (<= 0 u 1)
                  (<= 0 v 1)
                  (xy (+ x0 (* u (- x1 x0)))
                      (+ y0 (* u (- y1 y0))))))))))
]

Using this function, we can determine all intersections with the edges
of a polygon by doing

@lispcode[
(define (point-in-polygon? p vs)
  (let ((q (xy (+ (foldl max (cx (car vs)) (map cx vs)) 1)
               (cy p))))
  ...(map (lambda (vi vj)
            (intersection-segments p q vi vj))
          vs
          (append (cdr vs) (list (car vs))))))
]

The mapping result will be, for each pair of consecutive vertices
@${V_i-V_j}, an intersection point with the line @${P-Q} or
@lisp[#f] in the case where the intersection does not exist. Since we
only want to know the intersections, we can now filter this list,
keeping only those which are points, i.e., those which are not false:



@lispcode[
(define (point-in-polygon? p vs)
  (let ((q (xy (+ (foldl max (cx (car vs)) (map cx vs)) 1)
               (cy p))))
  ...(filter 
       (lambda (e) e)
       (map (lambda (vi vj)
              (intersection-segments p q vi vj))
            vs
            (append (cdr vs) (list (car vs)))))))
]

Now, to know how many intersections occur, we need only measure the
length of the resulting list and check if the result is odd:

@def[
(define (point-in-polygon? p vs)
  (let ((q (xy (+ (foldl max (cx (car vs)) (map cx vs)) 1)
               (cy p))))
    (odd?
      (length
        (filter 
          (lambda (e) e)
          (map (lambda (vi vj)
                 (intersection-segments p q vi vj))
               vs
               (append (cdr vs) (list (car vs)))))))))
]

@def/no-show[
(define (how-many? f lst1 lst2)
  (cond ((or (null? lst1) (null? lst2))
	 0)
	((f (car lst1) (car lst2))
	 (+ 1 (how-many? f (cdr lst1) (cdr lst2))))
	(else
	 (how-many? f (cdr lst1) (cdr lst2)))))
]

@questions[
@question{ The function @fn[point-in-polygon?] is not as efficient
  as it could be, since it performs a mapping followed by filtering only
  to count how many elements result in the final list. In practice,
  this combination of operations is no more than the counting of the number
  of times that a binary predicate is satisfied along successive
  elements of two lists. Define the operation @fn[how-many?] that
  implements this process. For example, consider:
	
 @incremental[(how-many? > '(1 5 3 4 6 2) '(1 6 2 5 4 3))]
}

@question{ In reality, the function @fn[how-many?] already exists
  in Racket with the name @fn[count]. Re-implement the function
  @fn[point-in-polygon?] so that it uses the function
  @fn[count].  }
]

With the function @fn[point-in-polygon?] it is now possible to
establish the association between each point and each polygon as these
occur in the plans provided by the municipal services. However, as we
mentioned initially, we need to be careful with the fact that it is
also possible, for a given polygon, that there is no associated point
or that more than one associated point may exist. One way of treating
all these situations identically will be to compute, for each polygon,
the list of points it contains. Ideally, this list should have only
one element, but in the case of there being errors in a plan, it could
have none or have more than one. In either case, the function always
returns a list, so there will never be an error.

To produce this list of points for a given polygon we must test each
of the plan's points to see if it belongs to the polygon. Now, this
is nothing more than a filtering of a list of points, keeping only those that
are contained in the polygon. Thus, admitting that @lisp[pts] is the
plan's list of points and @lisp[vs] are the vertices of a particular
polygon, we can define:


@def[
(define (points-in-polygon pts vs)
  (filter (lambda (pt)
            (points-in-polygon? 
              (xyz (cx pt) (cy pt) (cz (car vs)))
              vs))
          pts))
]

Finally, given a list of points representing the coordinates of
the top of the buildings and a list of polygons (each implemented by
the list of its vertices) representing the building's base perimeter, we
will create a three-dimensional representation of these buildings by
determining, for each polygon, which points it contains and then act
in accordance with the number of points found:

@itemize[
@item{If that number is zero, we do not know what the height of the building is. This represents a plan error that we might not want to fix or that we want to fix by simply arbitrating a height. For now, we will choose to do nothing.}
@item{If that number is one, then this point is located on top of the building and its @${z} coordinate gives us the corresponding height.}
@item{If that number is greater than one, then there are multiple applicable points, which can correspond to several possible situations:
  @itemize[
  @item{If one of the points in question also belongs to another polygon, then it is because there are polygons inside other polygons or polygons that intersect each other. The first case may correspond to a building that varies in height (for example, having an engine room at the top). The second case is, typically, an error in the plan.}
  @item{If the points all exclusively belong to the same polygon, then this can represent buildings with tops that are not horizontal planes.}
]
}]

The correct treatment of these latter cases is relatively complex. As
we only want to create a prismatic approximation to the shape of the
building, we will employ a simple approach: we use as
the building's height the highest coordinate found. The following
function implements this behaviour:


@def[
(define (creates-buildings points polygons)
  (for ((polygon polygons))
    (let ((pts (points-in-polygon points polygon)))
      (let ((n-pts (length pts)))
        (cond ((= n-pts 0)  ;;No existing points
               )
              ((= n-pts 1)  ;;Only one existing point
               (creates-building 
                 polygon
                 (cz (car pts))))
              (else         ;;Various existing points
               (creates-building 
                 polygon
                 (foldl max 
                        (cz (car pts)) 
                        (map cz (cdr pts))))))))))
]

@questions[
@question{ Another possible approach to calculate the height of a
  building corresponding  to a polygon that contains a multiple number of
  points is to use the @\emph{average} of the @${z} coordinates of these
  points. Implement this approach.  }
]


To create a building from the list of vertices of its perimeter and
its height we will simply create a polygonal region at the base of the
building that we then extrude to the top:


@def[
(define (creates-building vertices height)
  (if (> height 0)
    (extrusion (surface-polygon vertices)
               quota)
    #f))
]

Until now we have solved the problem only from a geometrical point of
view, representing the plan's points by its coordinates and the
polygons by the coordinates of its vertices. However, as we know, what the
CAD tool provides are geometrical entities and these are the ones that
contain the information we need. Now, given a list of entities,
selecting those that correspond to the points is nothing more than a
filtering process that only keeps the entities that satisfy the
predicate @fn[point?]. Given these entities, obtaining their
coordinates is nothing more than a mapping process with the function
@fn[primary-point]. This means that we can define the function that
obtains the coordinates of all points existing in a list of entities.


@def[
(define (points entities)
  (map point-position (filter point? entities)))
]

Likewise, given a list of entities, we can select the list of polygons'
vertices through a filtering process followed by a mapping
process, as follows:


@def[
(define (polygons entities)
  (map line-vertices
       (filter (lambda (ent)
                 (and (line? ent) (closed-line? ent)))
               entities)))
]

We should note that, in the previous definition, we are selecting both
the closed polygonal lines and the opened ones. This is for dealing
with the possibility of some polygonal lines being visually closed,
even if they are not, from the point of view of the geometrical
operation that was used to create them.

Finally, we are in conditions of defining a function which, from a plan's set of
entities, creates the corresponding three-dimensional representations:


@def[
(define (buildings<-entities entities)
  (creates-building (points entities) (polygons entities)))
]

Naturally, the set of entities to be processed can be selectively produced
or, alternatively, they can be all the entities
existing in a given plan. In the latter case, we only need to do:


@def[
(buildings<-entities (all-shapes))
]

As an example, we show in @figref{fig:AmostraCamara0} the
evaluation result of the previous expression for the plan presented
in @figref{fig:plantaCamara0}.


@figure[#:tag "fig:AmostraCamara0"
        #:caption @elem{Three-dimensional modelling of the buildings present in the plan shown in @figref{fig:plantaCamara0}.}]{
@autoimage{malhaUrbana}
}
