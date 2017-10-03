#lang scribble/manual
@(require "../rosetta.rkt")
@setup-math

@title{Parametric Representation}

@section{Introduction}

Until now, we have only produced curves described by functions in the
form @${y=f(x)}. These functions are said to be in the
@emph{Cartesian} form.

One other form of mathematically representing a curve is by means of
equations @${F(x,y)=0}. This form, called @emph{implicit}, is more
general than the former which, in fact, in nothing else than solving it
in terms of the ordinate @${y}. As an example of a curve described in the
implicit form let us consider the equation @${x ^ 2 + y ^ 2-r ^ 2 = 0}
which describes a circumference of radius @${r} centred at
@${(0,0)}. Unfortunately, this second way of representing curves is
not as useful as the previous, since it is not always trivial (or
possible) to solve the equation in terms of the
ordinate. @footnote{Except for lines, of which the general equation
is @${ax + by + c = 0, b \ neq 0} and where it is obvious that the
resolution in terms of the ordinate is @${y =
-\frac{ax+c}{b}}. For example, in the case of the circumference,
the best we can do is produce @emph{two} functions:

@$${y(x)=\pm\sqrt{r^2-X^2}}}

There is, however, a third form for representing of curves that
becomes particularly useful: the @emph{parametric} form. The
parametric form is based on the idea that the curve can be traced by
a point of which the position evolves over time. The time is, here, merely a
@emph{parameter} that determines the position of the point on the
curve. Resuming the case of the circumference centred at @${(0,0)}, its
parametric description will be

@$${x(t)= r\cos t}

@$${y(t)= r\sin t}

Obviously, for the coordinate point @${(x, y)} to trace the entire
circumference, the parameter @${t} needs only vary in the range
@${0,2\pi}.

The previous equations are called the @emph{parametric equations} of the
curve. If, in a parametric representation, we eliminate the parameter,
naturally we find the curve's original equation. For example, in
the case of the circumference, if we add the square of the parametric
equations we obtain

@$${x^2+y^2=r^2(\cos^2 t + \sin^2 t)=r^2}

To see a practical example, let us consider the @emph{Archimedean spiral}:
the curve described by a point which moves with constant
velocity @${v} along a radius that rotates around a pole with constant
angular velocity @${omega}. In polar coordinates, the equation of the
Archimedean spiral has the form

@$${\rho=\frac{v}{\omega}\phi}

or, with @${alpha=\frac{v}{\omega}},

@$${\rho=\alpha\phi}

When we convert the above equation to rectangular coordinates, using the formulas

@$$|{\left\{
\begin{aligned}
x&=\rho \cos \phi\\
y&=\rho \sin \phi
\end{aligned}\right.}|

we get

@$$|{\left\{
\begin{aligned}
x(\phi)&=\alpha\phi \cos \phi\\
y(\phi)&=\alpha\phi \sin \phi
\end{aligned}\right.}|

which, as can be seen, is a parametric description in terms of @${\phi}.

Simpler still is the direct conversion to the parametric form of the
polar representation of the Archimedean spiral. We only have to replace
@${phi} with @${t} and add one more equation that expresses this
change, that is

@$$|{\left\{
\begin{aligned}
 \rho(t)&=\alpha t\\
 \phi(t)&= t
\end{aligned}\right.}|

Although we explained the parametric representation in terms of 
bi-dimensional coordinates, the extension to three-dimensional space is 
trivial. For that, it is enough to consider each three-dimensional point a 
function of a parameter, i.e., @${(x,y,z)(t)=(x(t),y(t),z(t))}.


@section[#:tag "sec:parametrica"]{Computation of Parametric Functions}

Since the parametric representation significantly simplifies the
creation of curves, we now intend to define functions that, from
the parametric description of a curve, produce a list of points that
interpolate that curve. To generate these points we will specify the
limits of the interval of parameter @${[t_0, t_1]}, and the progressive increment
of its value @${\Delta_T}, to generate the sequence of values
@${t_0, t_0+\Delta_t, t_0+2\Delta_t,\ldots,t_1}. We have seen that
the function @fn[enumerate], defined in section @ref{sec:enumera} served precisely to
generate a list of these values:

@def[
(define (enumerate t0 t1 dt)
  (if (> t0 t1)
    (list)
    (cons t0
          (enumerate (+ t0 dt) t1 dt))))
]

For example, if we have @${t_0=1}, @${t_1=5}, and @${\Delta_t=1}, we
have:

@def[
> (enumerate 1 5 1)
'(1 2 3 4 5)
]

The computation of the coordinates of the Archimedean spiral can now be
trivially accomplished by mapping the function that gives us the positions onto
the list of values of @${t}:

@def[
(define (archimedean-spiral alpha t0 t1 dt)
  (map (lambda (t)
         (pol (* alpha t)
              t))
       (enumerate t0 t1 dt)))
]

In the previous definition, we see that the curve of the Archimedean
spiral is positioned at the origin. If we want to make the centre of a
spiral a parameter @lisp[p], we need only operate a simple
translation, i.e.:


@def[
(define (archimedean-spiral p alpha t0 t1 dt)
  (map (lambda (t)
         (+pol p
               (* alpha t)
               t))
       (enumerate t0 t1 dt)))
]

@Figref{fig:espiraisArquimedes} shows three  
Archimedean spirals drawn from the following invocations:

@def[
(spline
  (archimedean-spiral (xy 0 0) 2.0 0 (* 4 pi) 0.1))

(spline
  (archimedean-spiral (xy 50 0) 1.0 0 (* 6 pi) 0.1))

(spline
  (archimedean-spiral (xy 100 0) 0.2 0 (* 36 pi) 0.1))
]

@figure[#:tag "fig:espiraisArquimedes"
        #:caption @elem{Archimedean spirals. From left to right, the parameters are @${\alpha=2, t \in [0,4\pi]}, @${\alpha=1, t \in [0,6\pi]}, @${\alpha=0.2, t \in [0,36\pi]}.}]{
@(show-tikz 0.15)}


@section{Rounding errors}

Although very useful, the @fn[enumerate] function has a problem: when
the increment @lisp[dt] is not an integer, its successive addition
will accumulate rounding errors, which may cause a bizarre
behaviour. To exemplify the situation, let us consider the number of
elements of two enumerations of the interval @${[0,1]}, the first with an
increment of @${\frac{1}{10}} and the second with an increment of
@${\frac{1}{100}}:

@incremental[
(length (enumerate 0 1 1/10))
(length (enumerate 0 1 1/100))
]

Since the @fn[enumerate] function produces a list that includes both
the first and the last element of the interval, we conclude that the
number of generated elements is correct. However, if we replace
@${\frac{1}{10}} and @${\frac{1}{100}} by the equivalent @${0.1} and
@${0.01} we obtain something strange:


@incremental[
(length (enumerate 0 1 0.1))
(length (enumerate 0 1 0.01))
]

Obviously, there is a problem there: apparently the second enumeration ended
earlier than what would be expected, failing to produce the final
element. Worse still, this situation seems to have an irregular
behaviour, as we can see in the following interactions:


@incremental[
(length (enumerate 0 1 0.001))
(length (enumerate 0 1 0.0001))
(length (enumerate 0 1 0.00001))
(length (enumerate 0 1 0.000001))
]

The problem arises from the fact that @${0.1} and @${0.01} and are not
accurately representable in binary notation. In fact, @${0.1} is
represented by a number that is slightly smaller than
@${\frac{1}{10}}, while @${0.01} is represented by a number slightly
greater than the fraction @${\frac{1}{100}}. The consequence is that a
sufficiently large sum of these numbers eventually accumulates an
error that makes the final calculated value, after all, greater than
the limit, so it is discarded. @footnote{This phenomenon has been the
cause of countless problems in the computer science world. From errors in
anti-missile defence systems to the wrong calculation of stock market
indexes, the history of computer science is filled with catastrophic
examples caused by rounding errors.}

To avoid these problems it would be natural to think that we should
limit ourselves to using increments in the form of fractions but,
unfortunately, that is not always possible. For example, when we use
trigonometric functions (such as sines and cosines), it is usual to have to
generate values in a range that corresponds to a multiple of the
period of the functions that, as we know, involves the irrational
number @${\pi}, not representable as a fraction. Thus, to deal
with real numbers properly, we should use another approach
based on producing the number of actually desired values, avoiding
successive sums. For that, instead of using the increment
as a parameter, we will use the number of desired values instead.

In its actual form, from the limits @${t_0} and @${t_1} and the
increment @${\Delta_t}, the @fn[enumerate] function generates each of
the points @${t_i} by calculating:

@$${t_i=t_0+\underbrace{\Delta_t+\Delta_t+\cdots{}+\Delta_t}_{\text{$i$ vezes}} \equiv}
@$${t_i=t_0+\sum_{j=0}^{i}\Delta_t}

The problem of the previous formula is, as we have seen, the
potentially large error accumulation that occurs when we add
the small error that exists in the value of @${\Delta_t} a large
number of times. To minimize this error accumulation we have to find an
alternative formula that avoids the term @${\Delta_t}. An attractive
possibility is to consider, not an increment of @${\Delta_t}, but a
@${n} number of @${\Delta_t} increments that we are interested in computing.
Obviously, this @${n} number relates to @${\Delta_t} through the
equation

@$${n=\frac{t_1-t_0}{\Delta_T}}

Consequently, we also have

@$${\Delta_T=\frac{t_1-t_0}{n}}

Replacing in the previous equation, we obtain

@$${t_i=t_0+\sum_{j=0}^{i}\Delta_t \equiv}
@$${t_i=t_0+\sum_{j=0}^{i}\frac{t_1-t_0}{n} \equiv}
@$${t_i=t_0+\frac{t_1-t_0}{n}\sum_{j=0}^{i}1 \equiv}
@$${t_i=t_0+\frac{t_1-t_0}{n}i}

The fundamental aspect of the last equation is that it no longer
corresponds to a sum of @${i} terms where rounding error accumulation
can occur, but to a "function" @${f(i)=\frac{t_1-t_0}{n}i} of @${i},
where @${i} causes no error since it is simply an integer that
evolves from @${0} to @${n}. From this number it is now possible to
calculate the value of @${t_i} that, although it may have the
inevitable rounding errors caused by not using fraction numbers, it
will not have an accumulation of these errors.

Based on this last formula it is now easy to write a new function
@fn[range-n] that directly computes the value of @${t_i} from the
number @${n} of desired increments in the interval @${[t_0, t_1]}:


@def[
(define (enumerate-n t0 t1 n)
  (map (lambda (i)
         (+ t0 (/ (* i (- t1 t0)) n)))
       (enumerate 0 n 1)))
]

Naturally, this new definition does not eliminate rounding errors, but
it avoids their accumulation.

The @fn[enumerate-n] function is actually  pre-defined in Rosetta, and is
called @fn[division]. Similar to the function @fn[enumerate-n], the
@fn[division] function receives as parameters the interval limits and
the number of increments:

@incremental[(division 0 1 4) (division 0 pi 4)]

Different to the @fn[enumerate-n] function, the @fn[division] function
also has an optional parameter (by omission, the value @lisp[#t]) intended to
indicate whether we want to include the last element of the
interval. This option is intended to facilitate the use of periodic
functions. To better understand this parameter, let us consider that
we want to place four objects on the four cardinal points. For this we
can use polar coordinates, dividing the circle into four parts, with
the angles @${0}, @${\frac{\pi}{2}}, @${\pi}, and
@${\frac{3\pi}{2}}. However, if we use the expression
@lisp[(division 0 2pi 4)] we will not only get those values but also
the upper limit of the interval @${2\pi}, which would imply placing
two overlapping objects, one for the angle @${0} and another for
@${2\pi}. Naturally, we can solve the problem by using @lisp[(division 0
3pi/2 3)] but that seems much less natural than to write
@lisp[(division 0 2pi 4 #f)], that is, we want to divide @${2\pi} in four
pieces but do not want the last value because it will be equal to the
first (to less than one period).

@section{Mapping and Enumerations}

As we saw in the @fn[archimedean-spiral] function, generating
parametric curves implies mapping a function over a division of an
interval in equal parts. Because this combination is so frequent,
Rosetta provides a function called @fn[map-division] that performs
it in a more efficient way. Formally, we have:

@centered{
  @lisp[(map-division #,(lispemph f) #,(lispemphi t "0") #,(lispemphi t "1")
    #,(lispemph n))]@${\equiv}@lisp[(map #,(lispemph f) (division
    #,(lispemphi t "0") #,(lispemphi t "1") #,(lispemph n)))]
}

The @fn[map], @fn[division], and @fn[map-division] functions
allow us to generate curves with great simplicity. They are,
therefore, an excellent starting point for experimentation. In the
following sections we will look at some of the curves that, by one reason
or another, became part of the history of Mathematics.


@questions[
@question{ Redefine the @fn[archimedean-spiral] function that
  calculates a list of points through which an Archimedean spiral
  is traced, but using the @fn[division] function. Instead of the
  increment @${\Delta_T}, your function must, instead, receive the number
  @${n} of points.  }

@question{ Define the @fn[archimedean-spiral-wall] function that,
  from the thickness and height and also the parameters of an
  Archimedean spiral creates a spiral wall, as shown in the following
  image:

@fig[@autoimage[#:scale 0.7]{paredeEspiralArquimedes}]
}

@question{ Define the @fn[cylinders-archimedean-spiral] function that
  creates @${n} cylinders of radius @${r} and height @${h} and
  places them along an Archimedean spiral, in accordance with the
  parameters of this spiral, as presented in the following figure:
	
@fig[@autoimage[#:scale 0.7]{cilindrosEspiralArquimedes}]}
]


@subsection{Fermat's Spiral}

Fermat's spiral, also known as parabolic spiral, is a curve similar to
an Archimedean spiral but in which the equation that defines it is

@$${\rho^2=\alpha^2\phi}

Solving the equation for @${\rho}, we get

@$${\rho=\pm\alpha\sqrt{\phi}}


Dividing the curve into two halves for handling the two signals, 
we have:

@def[
(define (half-fermat-spiral p a t n)
  (map (lambda (t)
         (+pol p
               (* a (sqrt t))
               t))
       (division 0 t n)))

(define (fermat-spiral p a t n)
  (append (reverse (half-fermat-spiral p (+ a) t n))
          (cdr (half-fermat-spiral p (- a) t n))))]

To see an example of Fermat's spiral, we can evaluate the 
following expression:

@def[
(spline (fermat-spiral (xy 0 0) 1.0 (* 16 pi) 400))
]

The result of the previous evaluation is shown in @figref{fig:espiralFermat}.

@figure[#:tag "fig:espiralFermat"
        #:caption @elem{The Fermat's spiral for @${\phi \in [0, 16\pi]}.}]{
  @(show-tikz 0.7)}

An interesting aspect of the Fermat's spiral is that it models some
natural phenomena, in particular the arrangement of seeds on a
sunflower. On a sunflower, the seeds are arranged in circular surface and,
in order to achieve growing the greatest number of seeds
possible, they are leaning against each the other, in the most compact
way possible.

In these flowers, each seed is positioned according to the equations
@footnote{This model was proposed by H. Vogel in 1979 \cite{}.}

@$$|{\left\{
\begin{aligned}
\rho &= \alpha \sqrt{n} \\
\phi &= \delta \times n 
\end{aligned}\right.}|

where @${n} is the seed index counted from the centre of the flower
@footnote{This index is inversely proportional to the seeds' order of
growth.}, @${\alpha} is the scale factor and @${\delta} is the
@emph{golden angle} (also called Fibonacci angle) and defined by
@${\delta=\frac{\pi}{\Phi}^2} where @${\Phi=\frac{\sqrt{5}+1}{2}} is
the golden ratio. From the definition results that
@${\delta=2.39996\approx 2.4}.

We can see that this equation is identical to the Fermat's spiral, but with
the difference that the angle @${\phi} does not grow in terms of @${n}, but
instead in terms of @${\delta \times n}, moving every two seeds apart by
an angle of @${\delta}.
 
To visualize this curve, it is preferable to draw a "seed" in each
coordinate. Thus, we can define the function @fn[sunflower] that,
given a central point @lisp[p], a growth factor @lisp[a], an angle
@lisp[d], a radius @lisp[r] and a number of seeds @lisp[n], draws
the seeds of the sunflower with a circle of radius @lisp[r] for each
seed.

@def[
(define (sunflower p a d r n)
  (for/list ((t (division 1 n (- n 1))))
    (circle (+pol p
                  (* a (sqrt t))
                  (* d t))
             r)))
]

The following evaluations allow us to draw an approximation to the
arrangement of sunflower seeds:

@def/no-results[
(define golden-angle (/ 2pi (expt (/ (+ (sqrt 5) 1) 2) 2)))

(sunflower (xy 0 0) 1.0 golden-angle 0.75 200)
]

The result is visible in @figref{fig:girassol}.

@figure[#:tag "fig:girassol"
        #:caption @elem{The arrangement of Sunflower seeds.}]{
@tex{
\begin{tikzpicture}[scale=0.5]
\inputtikz{girassol}
\end{tikzpicture}
}}

It is interesting to note that the arrangement of seeds is extremely
sensitive to the @${\delta} angle. In @figref{fig:girassois}, the
sunflowers are designed in all ways identical to @figref{fig:girassol}
except in the @${\delta} angle that differs from
the golden angle @${\frac{\pi}{\Phi}^2} by just a few hundredths of a
radian.

@figure[#:tag "fig:girassois"
        #:caption @elem{The arrangement of imaginary sunflower seeds  wherein, from left to right, the @${\delta} angle corresponds to an increment, relative to the golden angle @${\frac{\pi}{\Phi}^2} of @${+0.03}, @${+0.02}, @${+0.01}, @${+0.00}, @${-0.01}, @${-0.02} e @${-0.03}.}]{
@tex{
\begin{tikzpicture}[scale=0.6]
\inputtikz{girassois}
\end{tikzpicture}
}}


@subsection{Cissoid of Diocles}

Legend has it that in the fifth century before Christ, the Greek
population was hit by the plague. Hoping for a response, the Greeks
resorted to the Oracle of Delos who prophesied that the problem was in
the incorrect veneration that was being given to the God
Apollo. According to the Oracle, Apollo would only be appeased if the
volume of the cube-shaped altar, that the Greeks had dedicated to him,
was doubled.

The Greeks, in an attempt to quickly get rid of the terrifying effects
of the plague, immediately embarked on the task of building a new
cubic altar, but, unfortunately, instead of duplicating its
@emph{volume}, they doubled its @emph{edge}, implying that the final
volume was eight times larger than the original volume, not just two
times larger as the oracle had commanded. Apparently, the oracle knew
what he was saying, because Apollo was not satisfied with the new
altar and the plague continued to devastate Greece.

After realizing their mistake, the Greeks then tried to figure out
what the correct change was that had to be made to the edge length of
the cube to double its volume but never managed to find a
solution. Doubling the volume of a cube then began to be called the
problem of Delos and for centuries it tormented
geometers. @footnote{Fortunately, the plague did not last as long as
the geometers' difficulties.} It was only two thousand years later,
by the hand of Descartes, that it was demonstrated that the Greek techniques
--- that limited the geometric construction to the use of a ruler and
a compass --- could never have solved the problem.
 
However, long before Descartes, in the second century before Christ,
the Greek mathematician Diocles had found a "solution" but at the
expense of the use of a special curve, now called the @emph{Cissoid of Diocles}.
This extraordinary curve, unfortunately, can not be drawn
just by using a ruler and compass, which implies that the solution found
would be, at best, an approximate solution and not the true solution
to the problem.
 
The Cissoid of Diocles is defined by the equation

@$${y^2(2a-x)=x^3, x\in [0, a[}

Placing @${y} in terms of @${x}, we obtain:

@$${y=\pm\sqrt{\frac{x^3}{2a-x}}}

Unfortunately, this formulation of the curve is somewhat inappropriate
because it forces us to separately handle the signals @${\pm}. A
conversion to polar coordinates allows us to obtain

@$${\rho^2\sin^2\phi(2a-\rho\cos\phi)=\rho^3\cos^3\phi}

Simplifying and using the Pythagorean identity @${\sin^2\phi+\cos^2\phi=1}, we obtain

@$${(1-\cos^2\phi)(2a-\rho\cos\phi)=\rho\cos^3\phi}

that is,

@$${2a(1-\cos^2\phi)-\rho\cos\phi+\rho\cos^3\phi=\rho\cos^3\phi}

Simplifying, we finally obtain

@$${\rho=2a(\frac{1}{\cos\phi}-\cos\phi)}

Since that @${\cos\pm\frac{\pi}{2}=0}, the curve tends to infinity for those
values. This delimits the range of variation to @${\phi \in
  ]-\frac{\pi}{2}, +\frac{\pi}{2}[}.

Obviously, to transform the polar representation into a  
parametric representation we simply do

@$$|{\left\{
\begin{aligned}
 \rho(t)&=2a(\frac{1}{\cos t}-\cos t)\\
 \phi(t)&= t
\end{aligned}\right.}|

Finally, to enable "centring" the curve in an arbitrary point, 
we carry out a translation from that point. Thus, to 
define this curve in Racket we just have to do:

@def[
(define (cissoid-diocles p a t0 t1 n)
  (map-division
     (lambda (t)
       (+pol p 
              (* 2 a (- (/ 1.0 (cos t)) (cos t)))
              t))
     t0
     t1
     n))
]

@Figref{fig:cissoidesDiocles} shows a sequence of Cissoids 
of Diocles created from the expressions:

@def/no-results[
(spline (cissoid-diocles (xy  0 0) 10.0 -0.65 0.65 100))
(spline (cissoid-diocles (xy  5 0)  5.0  -0.8  0.8 100))
(spline (cissoid-diocles (xy 10 0)  2.5  -1.0  1.0 100))
(spline (cissoid-diocles (xy 15 0)  1.0 -1.25 1.25 100))
(spline (cissoid-diocles (xy 20 0)  0.5  -1.4  1.4 100))
]

@figure[#:tag "fig:cissoidesDiocles"
        #:caption @elem{Cissoids of Diocles. From left to right the parameters are @${a=10, t \in [-0.65,0.65]}, @${a=5, t \in [-0.8,0.8]}, @${a=2.5, t \in [-1,1]}, @${a=1, t \in [-1.25 1.25]} e @${a=0.5, t \in [-1.4,1.4]}.}]{
@tex{
  \begin{tikzpicture}[scale=0.5]
    \inputtikz{cissoideDiocles}
  \end{tikzpicture}
}}


@subsection{Lemniscate of Bernoulli}

In 1694, Bernoulli published a curve to which he gave the name of
@emph{lemniscate}. That curve ended up becoming immensely famous
for its adoption as the symbol to represent infinity: 
@${\Large \infty}. Nowadays, the curve is known as lemniscate of
Bernoulli to distinguish it from other curves that have a similar
shape.

The Lemniscate of Bernoulli is defined by the equation

@${(x^2+y^2)^2=a^2(x^2-y^2)}

Unfortunately, as we discussed earlier, this form of analytical
representation of the curve is rather inappropriate for its drawing,
as it is difficult to put one variable in terms of the other. However,
if you apply the conversion from polar coordinates to rectangular
coordinates we get

@$${(\rho^2\cos^2 \phi + \rho^2\sin^2 \phi)^2=a^2(\rho^2\cos^2 \phi - \rho^2\sin^2 \phi)}
  
Dividing both terms by @${\rho^2} and using the trigonometric identities @${\sin^2
x + \cos^2 x=1} and @${\cos^2 x - \sin^2 x= \cos 2 x}, we obtain

@${\rho^2=a^2\cos 2 \phi}

This equation is now trivial to convert to the parametric form:

@$$|{\left\{
\begin{aligned}
 \rho(t)&=\pm a \sqrt{\cos 2 t}\\
 \phi(t)&= t
\end{aligned}\right.}|

Note that the presence of @${\pm} indicates that, in fact, 
two curves are being plotted simultaneously. To simplify, let us  
plot each of these curves independently. Thus, let us start 
by defining a function that calculates @emph{half} the lemniscate
with its origin at the point @lisp[p]:

@def[
(define (half-lemniscate-bernoulli p a t0 t1 n)
  (map-division
    (lambda (t)
      (+pol p
             (* a (sqrt (cos (* 2 t))))
             t))
    t0
    t1
    n))
]

Next, we define a function that draws a complete lemniscate  
from the drawing of two half lemniscates:

@def[
(define (graph-lemniscate-bernoulli p a t0 t1 n)
  (spline 
   (append (half-lemniscate-bernoulli p (+ a) t0 t1 n)
           (cdr (half-lemniscate-bernoulli p (- a) t1 t0 n)))))
]

We can now visualize the "infinity" produced by this curve
with the following expression:

@def/no-results[
(graph-lemniscate-bernoulli (xy 0 0) 1.0 -pi/4 pi/4 100)
]

The result is shown in @figref{fig:lemniscataBernoulli}.

@figure[#:tag "fig:lemniscataBernoulli"
        #:caption @elem{Lemniscate of Bernoulli.}]{
@tex{
  \begin{tikzpicture}[scale=0.5]
    \inputtikz{lemniscataBernoulli}
  \end{tikzpicture}
}}

@questions[
@question{ Even though in the previous examples we have used the parametric
  representation, for it being the most simple, this is not always
  the case. For example, the lemniscate of Gerono studied by the
  mathematician Camille-Christophe Gerono, is mathematically defined by
  the equation
  
  @$${x^4 - x^2 + y^2 = 0}
  
  Solving for @${y}, we obtain
  
  @$${y=\pm\sqrt{x^2-x^4}}
  
  To remain on the real plane, it is necessary
  that @${x^2-x^4\ge 0}, which implies @${x \in [-1,1]}. To avoid having
  to draw two curves (caused by the presence of the symbol @${\pm}), we
  could be tempted to use the parametric representation of this curve,
  defined by
  
@$$|{\left\{
\begin{aligned}
x(t)&=\frac{t^2-1}{t^2+1}\\
y(t)&=\frac{2t(t^2-1)}{(t^2+1)^2}
\end{aligned}\right.}|

Unfortunately, while the Cartesian representation only needs to range
@${x} between @${-1} and @${+1}, the parametric representation
needs to range @${t} from @${-\infty} to @${+\infty}, which raises
a computational impossibility. For this reason, in this case, the
Cartesian representation is preferable.

Taking these considerations into account, define the function
@fn[Graph-lemniscate-gerono] that draws the lemniscate of Gerono.
}
]

@subsection{Lamé Curve}

The Lamé curve is a curve for which the points satisfy the equation

@$${\left|\frac{x}{a}\right|^n\! + \left|\frac{y}{b}\right|^n\! = 1}

wherein @${n>0} and @${a} and @${b} are the curve radii.

The curve was studied in the nineteenth century by the French
mathematician Gabriel Lamé as an obvious generalization of the
ellipse. When @${n} is a rational number, the Lamé curve is called a
@emph{super-ellipse}. When @${n>2}, we obtain a hyperellipse,
the higher @${n} the closer to a rectangle the curve is. When
@${n<2} we get a hypoellipse, the smaller the @${n}, the closer to a
cross the curve is. For @${n=2}, we obtain an ellipse, and for
@${n=1}, we get a lozenge. These variations are visible in @figref{fig:Lame}
that shows variations of this curve for @${a=2},
@${b=1} and different values ​​of @${n}.

@figure[#:tag "fig:Lame"
        #:caption @elem{The Lamé curve for @${a=2}, @${b=1}, @${n=p/q}, @${p,q\in {1..8}}. The variable @${p} varies along the abscissas axis, while @${q} varies along the ordinates axis.}]{
@tex{
  \begin{tikzpicture}[scale=0.5]
    \inputtikz{lame}
  \end{tikzpicture}
}}

The Lamé curve became famous when it was proposed by Danish
scientist and poet Piet Hein as an aesthetic and functional compromise
between forms based on rectangular patterns and forms based on
curvilinear patterns. In his study for a street intersection in the
Sergels Torg neighbourhood, in Stockholm, where there was hesitation
between a traditional roundabout or a rectangular arrangement of
roads, Piet Hein suggested a superellipse as the intermediate form
between those two, producing the result that is visible in @figref{fig:sergels0}.
Of all the superellipses, the most
aesthetically perfect were, in Piet Hein's opinion, the ones parametrized by
@${n=\frac{5}{2}} and @${\frac{a}{b}=\frac{6}{5}}.

@figure[#:tag "fig:sergels0"
        #:caption @elem{The superellipse proposed by Piet Hein for Sergels square, in Stockholm. Photography Nozzman.}]{
@authorizedPhoto{superelipse/nozzman}
}

To draw the Lamé curve it is preferable we use the following 
parametric formulation.

@$$|{\left\{
\begin{aligned}
x\left(t\right) &= a\left(\cos^2 t\right)^{\frac{1}{n}} \cdot \sgn \cos t\\
y\left(t\right) &= b\left(\sin^2 t\right)^{\frac{1}{n}} \cdot \sgn \sin t
\end{aligned}\right. \qquad -\pi \le t < \pi}|

The translation of this formula to Racket is direct:

@def[
(define (super-ellipse p a b n t)
  (+xy p
        (* a (expt (expt (cos t) 2) (/ 1.0 n)) (sgn (cos t)))
        (* b (expt (expt (sin t) 2) (/ 1.0 n)) (sgn (sin t)))))
]

In order to generate a sequence of points of the superellipse we can,
as previously, use the @fn[map-division] function:


@def[
(define (superellipse-points p a b n pts)
  (for/list ((t (division -pi pi pts)))
    (super-ellipse p a b n t)))
]

Finally, to plot the superellipse, we can define:

@def[
(define (superellipse-curve p a b n pts)
  (spline
    (superellipse-points p a b n pts)))
]

The superellipses in @figref{fig:Lame} were generated using the
previous function. For this, we defined a function
@fn[test-superellipses] which, for values of @${a} and @${b} and for
a sequence of numbers, tests all the combinations of exponent @${n=p/q},
with @${p} and @${q} taking successive values from that sequence. For viewing
the super-ellipses each of them has an origin proportional to
the combination used of @${p} and @${q}:

@def[
(define (test-superellipses a b ns)
  (for ((num ns))
    (for ((den ns))
      (superellipse-curve
        (xy (* num (* 2.5 a)) (* den (* 2.5 b)))
        a
        b
        (/ num den)
        100))))
]

@Figref{fig:Lame} was directly generated from the evaluation of
the following expression:

@def[
(test-superellipses 2.0 1.0 (list 1 2 3 4 5 6 7 8))
]

@questions[
@question{
Define the function @fn[tank-superellipse] that creates a tank with a
superelliptical shape identical to the one of Sergels Torg's square,
as presented below:
  
@fig[@autoimage[#:scale 0.7]{tanqueSuperelipse}]

Suggestion: define a function named @fn[curved-wall] that builds walls
along a given curve. The function should receive the thickness and
height of the wall and an entity representing the curve (e.g., a
circle, a @emph{spline}, etc.). Consider using the function
@fn[sweep] defined in section @ref{sec:extrusions} as well.  }

@question{ Define the function @fn[circular-tanks] that builds a
  succession of circular tanks of which the centres are located along a
  superellipse, as presented in the following image:

@fig[@autoimage[#:scale 0.7]{tanquesCirculares}]

The function should receive the parameters of the superellipse, the
parameters of a circular tank and the number of tanks to create.  }

@question{ Define a function that, by invoking the functions
  @fn[tank-superellipse] and @fn[circular-tanks], creates a
  square as similar as possible to Sergels Torg's square, as shown
  in the following figure:
	

@fig[@autoimage[#:scale 0.7]{sergels}]
}

@question{ Modify the previous function to produce a variant that is
  perfectly circular:
	
@fig{@autoimage[#:scale 0.7]{sergelsCircular}}
}

@question{ Modify the previous function to produce the following
  variant:

@fig{@autoimage[#:scale 0.7]{sergelsAltern}}
}

@question{ Modify the previous functions to produce the following
  variant:

@fig{@autoimage[#:scale 0.7]{sergelsAltern2}}
}
]

@section{Precision}

Let us consider the butterfly curve, proposed by the mathematician
Temple H. Fay. This periodic curve is defined by the following
equation in polar coordinates:

@$${r=e^{\sin \phi} - 2 \cos (4 \phi ) + \sin^5\left(\frac{2 \phi - \pi}{24}\right)}

Similar to what we did before, we will consider the curve with respect to a
starting point @${P}, within a range of variation of the independent
parameter @${t \in [t_0, t_1]}.

@def[
(define (butterfly-curve p t0 t1 n)
  (map (lambda (t)
         (+pol p
               (+ (exp (sin t))
                  (* -2 (cos (* 4 t)))
                  (expt (sin (/ (- (* 2 t) pi) 24)) 5))
               t))
       (division t0 t1 n #f)))
]

To draw this curve we are left with determining the appropriate value of
the parameter @${n}. For this, it is important to realize that the
period of the Butterfly curve is @${24 \pi}, with the entire curve
generated for @${t \in [0, 24\pi]}. Since this period is relatively
large, the number @${n} of points to be used should be enough so that the
curve can be accurately reproduced and it is precisely here that
the greatest difficulty arises: without knowing the shape of the curve,
it is difficult to estimate which is the best value for @${n}. If the
value of @${n} is too low, the curve will not be faithfully
represented, in particular, in places where there are very accentuated
curvatures; if the value is too high, the greater the need for
computational resources.  In practice, some experimentation is
required to understand what best value is.
 
This need for experimentation relative to the value of number of points to
be used is clearly evident in @figref{fig:borboletas} which shows the Butterfly
curves drawn for the following increasing values ​​of @${n}:

@def/no-results[
(closed-spline (butterfly-curve (xy  0 10) 0 (* 24 pi)   10))
(closed-spline (butterfly-curve (xy 10 10) 0 (* 24 pi)   20))
(closed-spline (butterfly-curve (xy 20 10) 0 (* 24 pi)   40))
(closed-spline (butterfly-curve (xy 30 10) 0 (* 24 pi)   80))
(closed-spline (butterfly-curve (xy 40 10) 0 (* 24 pi)  160))
(closed-spline (butterfly-curve (xy  0  0) 0 (* 24 pi)  320))
(closed-spline (butterfly-curve (xy 10  0) 0 (* 24 pi)  640))
(closed-spline (butterfly-curve (xy 20  0) 0 (* 24 pi) 1280))
(closed-spline (butterfly-curve (xy 30  0) 0 (* 24 pi) 2560))
(closed-spline (butterfly-curve (xy 40  0) 0 (* 24 pi) 5120))
]

@figure[#:tag "fig:borboletas"
        #:caption @elem{The butterfly curve produced for increasing values of the number @${n} of points.}]{
@tex{
  \begin{tikzpicture}[scale=0.5]
    \inputtikz{borboletas}
  \end{tikzpicture}
}}

As we can verify, when the number of points is insufficient, the
produced curves can be grotesque distortions of reality. For this
reason, we should take particular care in choosing the value most suitable
for this parameter.

An interesting alternative is to give the computer the responsibility
of adapting the number of points to the necessities of each curve.


@subsection{Adaptive Sampling}

The @fn[map-division] function allows us to generate the curve of
any function @${f(t)} in the interval @${[t_0, t_1]} by applying the
function @${f} to a sequence of @${n} increments of @${t} equally
spaced @${\{t_0,\ldots,t_1\}}. This sequence is named sampling
sequence.

It is easy to see that the greater the number of the sampling sequence
elements, the greater the accuracy of the produced curve will be.
Unfortunately, the greater that accuracy, the greater the effort expended
for computing the function values in all elements of the
sequence. In practice, it is necessary we find a balance between
accuracy and computational effort.

Unfortunately, there are several functions for which it is difficult
or even impossible to find an appropriate number of sampling points. As an
example, let us try to produce the graph of the function
@${f(t)=(t,\frac{1}{2}sin(\frac{1}{t}))} in the interval
@${[0.05,0.8]}. This function has a curve for a graph that oscillates
with a frequency that is greater the closer it is to the
origin. @Figref{fig:grafsininvlin} shows a sequence of graphs
of this function in which, from left to right, the number of sampling
points was progressively increased.

@figure[#:tag "fig:grafsininvlin"
        #:caption @elem{Graph of the function @${f(x)=\frac{1}{2}sin(\frac{1}{x})} in the interval @${[0.05,0.8]}. From left to right the sampling points @${8,15,30} and @${60} were employed.}]{
@tex{
\begin{tikzpicture}[scale=3.0]
\inputtikz{graficoFuncao}
\end{tikzpicture}  
}}

As we can see in the figure, with few sampling points the resulting
curve is a grotesque approximation of the real curve and it is only when
the number of sampling points becomes very high that some precision is acquired.
It so happens that that precision is only relevant in parts of the curve where
there are large variations. In other areas where the curve evolves "smoothly"
more sampling points only imply greater processing time, without any significant advantage to
the quality of the result. This suggests that we should adapt the
number of sampling points along the curve according to its
"smoothness": in areas where the curve varies more sharply we should
employ more sampling points, but in areas where the variation is more
linear, we can decrease the number of sampling points. This way, the
number of sampling points adapts to shape of the curve.

@figure[#:tag "fig:geradapt"
        #:caption @elem{Adaptive generation of points}]{
@tex{
\begin{tikzpicture}
\inputtikz{adaptativa}
\end{tikzpicture}  
}}

For us to employ an adaptive sampling we need to define a criteria to
classify the "smoothness" of a curve. As illustrated in
@figref{fig:geradapt}, we can start by admitting that we have two
abscissae @${x_0} and @${x_1} that we use to calculate the points
@${P_0} and @${P_1}. To know if the curve behaves in a linear fashion
between those two points (and thus can be approximated by a line
segment), we shall calculate the average value @${x_m} of that interval and the
corresponding point @${P_m} of the curve. If the function is in fact
linear in that interval, then the points @${P_a} and @${P_m} are
collinear. In practice, that will rarely happen, so we will have to
use a concept of @emph{approximate} collinearity, for which we can
use any of the several possible criteria, such as:


@itemize[
@item{The area of the triangle @${P_0,P_m,P_1} to be smaller than a sufficiently small value of @${\epsilon} .}
@item{The angle @${P_0,P_m,P_1} to be approximately equal to @${\pi}.}
@item{The sum of the distance between @${P_0} and @${P_m} with the distance between @${P_m} and @${P_1} to be approximately equal to the distance between @${P_0} and @${P_m}.}
]

Other criteria would be equally applicable, but for now, let us choose the
first. Its translation into Racket is as follows:

@def[
(define (approximately-collinear? p0 pm p1 epsilon)
  (let ((a (distance p0 pm))
        (b (distance pm p1))
        (c (distance p1 p0)))
    (< (area-triangle a b c) epsilon)))

(define (area-triangle a b c)
  (let ((s (/ (+ a b c) 2.0)))
    (sqrt (* s (- s a) (- s b) (- s c)))))
]

The @fn[approximately-collinear?] function implements the criteria
that allows us to say that the line segments connecting the points
@lisp[p0], @lisp[pm], and @lisp[p1] are a good approximation to the
curve's behaviour between those points. When this criteria does not
occur, we can split the interval into two sub-intervals, and analyse each
of them separately, concatenating the results.


@def[
(define (map-division-adapt f t0 t1 epsilon)
  (define p0 (f t0))
  (define p1 (f t1))
  (define tm (/ (+ t0 t1) 2.0))
  (define pm (f tm))
  (if (approximately-collinear? p0 pm p1 epsilon)
    (list p0 pm p1)
    (append (map-division-adapt f t0 tm epsilon)
            (cdr (map-division-adapt f tm t1 epsilon)))))
]

This adaptive way of producing a sample of a curve allows results with
greater precision even when employing a relatively small total number
of sampling points.  @Figref{fig:grafsininvadapt} shows a sequence
of graphs identical to those shown in @figref{fig:grafsininvlin},
but now using an adaptive sampling scheme with approximately the same
number of sampling points in each case. As we can see, the graphs
are substantially more accurate, particularly in areas close to the
origin, without this visibly diminishing the quality of the curve in
areas where the change is smoother.

@figure[#:tag "fig:grafsininvadapt"
        #:caption @elem{Graph of the function @${f(x)=\frac{1}{2}sin(\frac{1}{x})} in the interval @${[0.05,0.8]}. From left to right, the sequence of adaptive sampling has, respectively, @${7,15,29} and @${57}	points.}]{
@tex{
\begin{tikzpicture}[scale=3.0]
\inputtikz{grafsininvadapt}
\end{tikzpicture}
}}

@questions[
@question{ The algorithm used in the
  @fn[map-division-adapt] function can not properly deal with periodic
  functions in which we have @${f(t_0)=f(t_1)=f(\frac{t_0+t_1}{2})}. In this
  case, the degenerate triangle formed by these three points has zero
  area, which makes the algorithm terminate immediately. Modify the
  function so it receives an additional parameter @${\Delta_T}
  that determines the biggest admissible interval between two
  consecutive values of @${t}.
	
}

@question{ The @fn[map-division-adapt] function is not as efficient
  as it could be, in particular, because it systematically
  repeats the application of the function @${f} at the midpoint:
  @${f(t_m)} is calculated to decide whether the recursion continues and,
  when it continues, it will have to be calculated again to allow calculating
  @${f(t_0)} or @${f(t_1)} on the following call. Redefine the
  @fn[map-division-adapt] function in order to avoid repetitive
  calculations.  }
]



@section{Parametric Surfaces}

Throughout history, architecture has explored a huge variety of
forms. As we have seen, some of these forms correspond to solids
well known since antiquity, such as cubes, spheres and cones. More
recently, architects have turned their attention for much less
classical forms, which require a different kind of description. The
famous Spanish architect Felix Candela is a good example.

Candela thoroughly explored the properties of the hyperbolic
paraboloid, allowing him to create works that are now references in
the world of architecture. @Figref{fig:felix} illustrates the
construction of one of the works by Félix Candela, where the fundamental
element is precisely a thin concrete shell in the shape of a
hyperbolic paraboloid. In his time, Candela performed all
necessary calculations manually to determine the forms he intended to create.
We will now see how we can get the same results in a much more efficient
way.

@figure[#:tag "fig:felix"
        #:caption @elem{The Lomas de Cuernavaca Chapel under construction. Photography by Dorothy Candela.}]{
@authorizedPhoto{candela/Chapel-Lomas-de-Cuernavaca}
}

For this, we are going to generalize the parametric descriptions of
curves so as to allow the specification of surfaces. Even though a curve
is described by a triple of functions of a single parameter @${t}, for
example, @${(x(t), y(t), z(t))}, in the case of a surface it will have to
be described by a triple of function of two parameters that vary independently,
@footnote{Mathematically speaking, a parametric surface is simply the
image of an injective function of @${\mathbb R^2} in @${\mathbb
R^3}.} for example, @${(x(u,v), y(u,v), z(u,v))}. Obviously, the
choice of rectangular, cylindrical, spherical coordinates or others is
just a matter of convenience: in any case, three functions are
required.

As it happens with curves, although we can describe a surface in an implicit
form, the parametric representation is more adequate for the generation of the
positions through which the surface passes. For example, a sphere
centred at point @${(x_0, y_0, z_0)} and with radius @${r}, can be
described implicitly by the equation

@$${(x-x_0)^2+(y-y_0)^2+(z-z_0)^2-r^2=0}

but this form does not allow
the direct generation of coordinates, being more preferable the adoption of a
parametric description, by the functions

@$$|{\left\{
  \begin{aligned}
    x(\phi,\psi) &= x_0 + r \sin\phi\cos\psi\\
    y(\phi,\psi) &= y_0 + r \sin\phi\sin\psi\\
    z(\phi,\psi) & =z_0 + r \cos\phi
  \end{aligned}\right.}|

To illustrate the concept of parametric surface, we are going to
consider a famous surface: the Möbius Strip @footnote{The Möbius Strip
is not a true surface, but a @emph{surface with boundary.}}

@subsection{The Möbius Strip}

The Möbius strip (also known as Möbius @emph{band}) was described
independently by two German mathematicians in 1858, first by Johann
Benedict Listing and two months later by August Ferdinand
Möbius. Although Listing was the only one who published this discovery, it was
eventually baptised with the name of Möbius.  @Figref{fig:moebius0} shows a
picture of a Möbius band made from a paper strip and where it is
possible to see one of the extraordinary features of this surface, it
is possible to walk around it entirely without ever leaving the same
"side" because, in truth, the Möbius strip only has one
side. @footnote{This property has been exploited, for example, in
transmission belts that, when adopting the topology of a Möbius
strip, they wear out both "sides" of the belt instead of only one,
thereby making them last twice as long.}  Likewise, the
surface is limited only by one edge, which extends to the entire
surface. In fact, the Möbius strip is the simplest surface of only one
side and one edge.


@figure[#:tag "fig:moebius0"
        #:caption @elem{The Möbius Strip.}]{
@authorizedPhoto{moebius/MobiusStrip}
}

The Möbius strip has been successively used in various contexts, from
the famous works of Maurits Cornelis Escher that employ a Möbius strip,
as "Möbius Strip II", to the representative symbol of the
@emph{recycling}, which consists of three arrows arranged along a
Möbius strip, as can be see in @figref{fig:reciclagem}.

@figure[#:tag "fig:reciclagem"
        #:caption @elem{The representative symbol of @emph{recycling}: three arrows arranged along a Möbius strip.}]{
@authorizedPhoto{moebius/recycle}
}

The parametric equations of this surface, in cylindrical coordinates, 
are as follows:

@$$|{\left\{
  \begin{aligned}
    \rho(s,t)& =1 + t \cos\frac{s}{2}\\
    \theta(s,t)& =s\\
    z(s,t)& =t \sin\frac{s}{2}
  \end{aligned}\right.}|


Given the periodicity of the trigonometric functions, we have that
@${\frac{s}{2} \in [0,2\pi]} or @${s \in [0,4\pi]}. In that
period, for a given @${t}, the @${z} coordinate will assume
symmetrical values in relation to the @${xy} plane, in the limit,
between @${-t} and @${t}. This means that the Möbius strip develops up
and down the @${xy} plane.

To draw this surface we can use, as an initial approach, a
generalization of the process we used to draw parametric
curves. In this approach, we simply applied the argument function
@${f(t)} along a succession of @${n} values ​​from @${t_0} to
@${t_1}. In the case of parametric surfaces, we intend on applying the
function @${f(s,t)} over a succession of @${m} values ​​from @${s_0} to
@${s_1}, and for each one, over a succession of @${n} values, from
@${t_0} to @${t_1}. Thus, the @${f(s,t)} function will be applied to
@${m\times n} pairs of values. This behaviour is easily obtained at
the expense of two chained mappings:


@lispcode[
(map (lambda (s)
       (map (lambda (t)
              (f s t))
            (division t0 t1 n)))
     (division s0 s1 m))
]

For example, in the case of Möbius strip, we can define:

@def[
(define (mobius-strip s0 s1 m t0 t1 n)
  (map (lambda (s)
         (map (lambda (t)
                (cyl (+ 1 (* t (cos (/ s 2))))
                     s
                     (* t (sin (/ s 2)))))
              (division t0 t1 m)))
       (division s0 s1 n)))
]

In fact, this combination of mappings with interval divisions is so
frequent that Rosetta already provides it through an extension of the
@fn[map-division] function, allowing us to write

@def[
(define (mobius-strip s0 s1 m t0 t1 n)
  (map-division (lambda (s t)
                  (cyl (+ 1 (* t (cos (/ s 2))))
                       s
                       (* t (sin (/ s 2)))))
                s0 s1 m t0 t1 n))
]

We have seen that in the case of a function with a single
parameter @${f(t)}, we had that

@lispcode[
(map-division #,(lispemph f) #,(lispemphi t "0") #,(lispemphi t "1") #,(lispemph n))
]
was equivalent to

@lispcode[
(map #,(lispemph f)
     (division #,(lispemphi t "0") #,(lispemphi t "1") #,(lispemph n)))
]

This equivalence is now extended to the case of functions that have
two parameters @${f(s,t)}, that is,

@lispcode[
(map-division #,(lispemph f) #,(lispemphi s "0") #,(lispemphi s "1") #,(lispemph m) #,(lispemphi t "0") #,(lispemphi t "1") #,(lispemph n))
]

is equivalent to

@lispcode[
(map (lambda (s)
       (map (lambda (t)
              (#,(lispemph f) s t))
            (division #,(lispemphi t "0") #,(lispemphi t "1") #,(lispemph n))))
     (division #,(lispemphi s "0") #,(lispemphi s "1") #,(lispemph m)))
]

Naturally, if the interior mapping produces a list with the results of the
application of the function @${f}, the exterior mapping will produce a
list of lists of results of the application of the function
@${f}. Assuming that each application of the function @${f} yields a
point in three-dimensional coordinates, we can easily see that this
expression produces a list of lists of points. Thus, the result will be
in the form of:


@lispcode[
((#,(lispemphi P "0,0") #,(lispemphi P "0,1") ... #,(lispemphi P "0,m"))
 (#,(lispemphi P "1,0") #,(lispemphi P "1,1") ... #,(lispemphi P "1,m"))
 ...
 (#,(lispemphi P "n,0") #,(lispemphi P "n,1") ... #,(lispemphi P "n,m")))
]

To draw this list of lists of points, we can simply 
apply the @fn[spline] function to each of the lists of points:


@def[
(define (splines ptss)
  (map spline ptss))
]

Thus, we can easily represent a first preview of the Möbius strip:

@def/no-results[
(splines (mobius-strip 0 4pi 80 0 0.3 10))
]

The result of the evaluation of the above expression is represented in  
@figref{fig:moebius1}.

@figure[#:tag "fig:moebius1"
        #:caption @elem{The Möbius strip.}]{
@tex{
  \begin{tikzpicture}[scale=0.6]
    \inputtikz{moebiusn0}
  \end{tikzpicture}
}}

An interesting aspect of three-dimensional parametric surfaces is that
the set of points that define them can be arranged in a matrix. In
fact, if the parametric surface is obtained by applying the function
@${f(s,t)} over a succession of @${m} values, from @${s_0} to
@${s_1}, and for each one, over a succession of @${n} values ​​from
@${t_0} to @${t_1}, we can place the results of those applications in a
matrix of @${m} rows and @${n} columns, as follows:

@$${\begin{bmatrix} 
f(s_0,t_0) & \dots & f(s_0,t_1)\\
\dots      & \dots & \dots\\
f(s_1,t_0) & \dots & f(s_1,t_1)
\end{bmatrix}}

What the @fn[map-division] function returns is nothing more than a
representation of this matrix, implemented in terms of a list of
matrix rows, each row implemented in terms of a list of values
corresponding to the elements of that row of the matrix.

It is now easy to see that the application of the function
@fn[splines] to the representation of the matrix does nothing else
than to draw @${m} @emph{splines}, each defined by @${n} points of
each of the @${m} rows of the matrix. Another alternative is to draw @${n}
@emph{splines} defined by @${m} points of each of the @${n} columns of the
matrix.  For this, the simplest way is to @emph{transpose} the matrix,
i.e., change the lines with the columns, in the form:


@$${\begin{bmatrix} 
f(s_0,t_0) & \dots & f(s_1,t_0)\\
\dots      & \dots & \dots\\
f(s_0,t_1) & \dots & f(s_1,t_1)
\end{bmatrix}}

For this, we can define a function that, given a matrix implemented as
a list of lists, returns the transposed matrix in the same
implementation:


@def[
(define (transposed-matrix matrix)
  (if (null? (car matrix))
      (list)
      (cons (map car matrix)
            (transposed-matrix (map cdr matrix)))))
]

Using this function, it is now possible to trace orthogonal curves to
the ones represented in @figref{fig:moebius1} simply by tracing
the obtained curves from the transposed matrix, i.e.


@def/no-results[
(splines (transposed-matrix (mobius-strip 0 4pi 80 0 0.3 10)))
]

@figure[#:tag "fig:moebius2"
        #:caption @elem{The Möbius strip.}]{
@tex{
  \begin{tikzpicture}[scale=0.6]
    \inputtikz{moebiusn1}
  \end{tikzpicture}
}}

The result is shown in @figref{fig:moebius2}.

Finally, to obtain a @emph{mesh} with the combined effect of both
curves, we can define an auxiliary function @fn[mesh-splines]:


@def[
(define (mesh-splines ptss)
  (splines ptss)
  (splines (transposed-matrix ptss)))
]

We can now use this function as follows:

@def/no-results[
(mesh-splines (mobius-strip 0 4pi 80 0 0.3 10))
]

@figure[#:tag "fig:moebius3"
        #:caption @elem{The Möbius strip.}]{
@tex{
  \begin{tikzpicture}[scale=0.6]
        \inputtikz{moebiusn0}
  	\inputtikz{moebiusn1}
  \end{tikzpicture}
}}

The result is shown in @figref{fig:moebius3}. 

This type of representation based on the use of lines to give the
illusion of an object, is called @emph{wireframe}. Formally, a wireframe
is nothing more than a set of lines that represent a surface. Wireframe models
have the advantage, over other forms of visualization, of
allowing a much faster drawing.


@section{Surfaces}

Wireframe models do not constitute surfaces. In fact, as the lines are
infinitely thin and there is no "material" between them, it is not
possible to associate a wireframe model to a real surface.

If what we want is, in fact, to draw surfaces, then it is preferable
we exploit the capabilities of Rosetta for the creation of surfaces.

Polygon meshes are clusters of polygons, usually triangles and
quadrilaterals which define the shape of an object. In comparison to models
constituted only by lines, polygon meshes have the advantage of
being more realistically displayed, for example, with the removing of invisible
surfaces, with the inclusion of shading, etc. Obviously, each face of
these polygonal meshes has zero thickness, therefore they are not actually
solids but only abstract representations of surfaces. Even so, they
can be very useful to obtain a more correct visualization of a surface and
can be subsequently transformed for creating solids, for example, using
the function @fn[thicken] that gives the surface a uniform
thickness.

Rosetta provides an operation for creating these polygonal meshes
named @fn[surface-grid]. @Figref{fig:moebius-render0} shows a
realistic representation of the Möbius strip generated from the
following expression:


@def[
(surface-grid (mobius-strip 0 4pi 80 0 0.3 10))
]

@figure[#:tag "fig:moebius-render0"
        #:caption @elem{The Möbius strip.}]{
@autoimage{moebius0}
}

It is now trivial to experiment with variations of the Möbius strip, for example,
to vary the strip's "width", as depicted in @figref{fig:moebius-render1}.

@figure[#:tag "fig:moebius-render1"
        #:caption @elem{The Möbius strip for different "widths". From left to right the variation interval in @${v} is @${[0,\frac{1}{2}]}, @${[0,1]}, @${[0,2]} and @${[0,4]}.}]{
@autoimage{moebius1}
}

To visualize a more architectural example, let us consider the
hyperbolic paraboloid used by Félix Candela in the Lomas Cuernavaca
Chapel. This surface can be described in its implicit representation
by the equation

@$${x^2-y^2-z=0}

or, likewise, through its parametric representation:


@$$|{\left\{
  \begin{aligned}
    x(s,t)& = s\\
    y(s,t)& = t\\
    z(s,t)& = s^2 - t^2
  \end{aligned}\right.}|
  
The translation to Rosetta is trivial:


@def[
(thicken
  (surface-grid
    (map-division
      (lambda (x y)
        (xyz x y (- (* x x) (* y y))))
      -0.5 1.3 40
      -2 2 80))
  0.03)
]

In the previous expression, we used the domain limits that
approximate the surface to the real work and we gave thickness to the
surface through the function @fn[thicken]. In @figref{fig:cuernavaca}
we show a view of the work where we eliminated the portion that is under the earth.


@figure[#:tag "fig:cuernavaca"
        #:caption @elem{An approximation of the Lomas Cuernavaca Chapel.}]{
@autoimage{lomasCuernavaca}
}

@questions[
@question{ Define a function that, with appropriate arguments, builds
  the surface shown in the following figure.

@fig[@autoimage{moebius2}]
}

@question{
Consider the following image:

@autoimage{sinuxv}

Assume that the figure is centred at the origin and represented using
an isometric perspective. Notice that the surface intersections with any
plane parallel to the @${XZ} plane or to the @${YZ} plane produces a
perfect sinusoid (though it produces sinusoids of different
frequencies depending on the distance from the intersection plane to
the origin). From these tips, try to figure out the parametric
equations that originated it.  }
]

@subsection{Helicoid}

We discussed, in section @ref{sec:helix}, the drawing of the helix. Let us
now discuss the surface that generalizes this curve: the
@emph{helicoid}.

The helicoid was discovered in 1776 by Jean Baptiste Meusnier. Its
parametric equations (in cylindrical coordinates) are:

@$$|{\left\{
  \begin{aligned}
    \rho(u,v)& =u\\
    \theta(u,v)& =\alpha v\\
    z(u,v)& =v
  \end{aligned}\right.}|

@Figref{fig:helicoide-render0} shows two helicoids with
different parameters, generated from the following fragment of program:


@figure[#:tag "fig:helicoide-render0" #:caption @elem{Two helicoids with
variation intervals in @${v} of @${[0,2\pi]}. The left helicoid has
@${\alpha=1}, and variation interval in @${u} of @${[0,1]} while the right one
has @${\alpha=3} and @${u \in [0,5]}.}]{ @autoimage{helicoide}}

@def[
(define (helicoid p a u0 u1 m v0 v1 n)
  (map-division
    (lambda (u v)
      (+cyl p
            u
            (* a v)
            v))
    u0 u1 m v0 v1 n))

(surface-grid (helicoid (xyz 0 0 0) 1 0 1 10 0 2pi 100))
(surface-grid (helicoid (xyz 7 0 0) 3 0 5 50 0 2pi 200))
]

An interesting feature of the helicoid is that for each point of 
the helicoid there is a helix that passes though it.

@subsection{Spring}

A @emph{spring} is another geometric figure that has affinities with
an helix. Mathematically, the spring is the volume generated by a
circle that travels along an helix. In simpler terms, a spring is an
helix with "thickness", i.e., a tube that wraps around an
axis. If @${r_0} is the radius of the tube, @${r_1} the distance from
the tube axis to the helix axis, @${\alpha} the initial
rotation angle of the spring and @${v_z} the "speed" along the @${z} axis,
the parametric equations of the spring are:


@$$|{\left\{
  \begin{aligned}
    \rho(u,v)& =r_1+r_0\cos u\\
    \theta(u,v)& =\alpha+v\\
    z(u,v)& = \frac{v_z v}{\pi} + r_0\sin u
  \end{aligned}\right.}|

The translation of the above definition into Racket is:

@def[
(define (spring p a r0 r1 vz u0 u1 m v0 v1 n)
  (map-division
    (lambda (u v)
      (+cyl p
            (+ r1 (* r0 (cos u)))
            (+ a v)
            (+ (/ (* vz v) pi) (* r0 (sin u)))))
    u0 u1 m v0 v1 n))
]

In @figref{fig:springs-render0} we find visualizations of a 
spring in its extension process. Each image corresponds to
a spring with the same dimensions, except in regards to the 
"speed" in @${z}.


@def[
(surface-grid
  (spring (xyz  0 0 0) 0 1 5 1 0 2pi 50 0 (* 3 2pi) 150))
(surface-grid
  (spring (xyz 20 0 0) 0 1 5 2 0 2pi 50 0 (* 3 2pi) 150))
(surface-grid
  (spring (xyz 40 0 0) 0 1 5 4 0 2pi 50 0 (* 3 2pi) 150))
]

@figure[#:tag "fig:springs-render0"
        #:caption @elem{Three springs created with the parameters @${r_0=1}, @${r_1=5}, @${u \in [0, 2\pi]} and @${v \in [0, 6\pi]}. From left to right, we have the speed @${vz \in \{1,2,4\}}.}]{
@autoimage{molas}
}

@questions[
@question{ The traditional spring is just the simplest form that is
  possible to generate from the function @fn[spring]. More
  elaborate shapes can be generated by either parametrization, or by
  composition. For example, by composing "springs" we can generate
  @emph{ropes}. In this case, we treat the "springs" as "strands" or
  "filaments" which, wrapped around each other, constitute a rope.
	
  Thus, a rope can be seen as a set of intertwined springs, with all
  the springs developing along the same axis but "rotated" (i.e., with an
  initial angle) and a step (i.e., a velocity of development) which
  allows them to be close to each other. In the following
  Figure we have represented two diagrams that show, on the left,
  the position of the springs in the case of a rope with four "strands". This
  rope can be generalized from any number of strands, to the lower limit
  of two strands, shown on the right.

@fig{@tex{
\begin{tikzpicture}[scale=0.8]
\inputtikz{corda}
\end{tikzpicture}
}}

Define the function @fn[rope] that, by receiving the appropriate
parameters, creates ropes. For example, consider the following
expressions of which the evaluation produces three ropes built with two,
three and six strands, shown below:


@lispcode[
(rope (xyz  0 0 0) 2 1 3)
(rope (xyz 10 0 0) 3 1 2)
(rope (xyz 20 0 0) 6 1 1)
]

@autoimage{cordas}
}


@question{
A @emph{breather} is a mathematical surface that characterizes a 
particular type of wave which is illustrated in the following Figure. Its parametric equation is::

@$$|{\left\{
  \begin{aligned}
x(u,v)& = -u+\frac{2\left(1-a^2\right)\cosh(au)\sinh(au)}{a\left(\left(1-a^2\right)\cosh^2(au)+a^2\,\sin^2\left(\sqrt{1-a^2}v\right)\right)} \\
y(u,v)& = \frac{2\sqrt{1-a^2}\cosh(au)\left(-\sqrt{1-a^2}\cos(v)\cos\left(\sqrt{1-a^2}v\right)-\sin(v)\sin\left(\sqrt{1-a^2}v\right)\right)}{a\left(\left(1-a^2\right)\cosh^2(au)+a^2\,\sin^2\left(\sqrt{1-a^2}v\right)\right)} \\
z(u,v)& = \frac{2\sqrt{1-a^2}\cosh(au)\left(-\sqrt{1-a^2}\sin(v)\cos\left(\sqrt{1-a^2}v\right)+\cos(v)\sin\left(\sqrt{1-a^2}v\right)\right)}{a\left(\left(1-a^2\right)\cosh^2(au)+a^2\,\sin^2\left(\sqrt{1-a^2}v\right)\right)}
  \end{aligned}\right.}|

@autoimage[#:scale 0.8]{breather}

Note that due to the periodicity of the trigonometric functions, we have that
@${-2\pi\leq u \leq 2\pi} and @${-12\pi\leq v \leq 12\pi}. The @${a} parameter
characterizes different surfaces and may vary between @${0} and @${1}.

Define the function @fn[breather] that draws the surface in
question from a point @${p}, the parameter @${b} and the variation
limits and the number of points to consider along that variation,
respectively, for the parameters @${u} and @${v}. For example, the
previous figure was produced by evaluating the expression:

@lispcode[
(surface-grid
  (breather (xyz 0 0 0) 0.4 -13.2 13.2 200 -37.4 37.4 200))
]
}
]

@subsection{Shells}

Several authors argue that nature is a mathematical manifestation and
justify this belief by the striking resemblance that some mathematical
surfaces exhibit with natural forms.

The Dini surface, is one such cases. The parametric equations 
that define it are:


@$$|{\left\{
  \begin{aligned}
    x(u,v)& =\cos(u)\sin(v)\\
    y(u,v)& =\sin(u)\sin(v)\\
    z(u,v)& =\cos(v)+\log(\tan(\frac{v}{2}))+a*u
  \end{aligned}\right.}|

where @${0\leq u\leq 2\pi} and @${0\leq v\leq\pi}.

The translation of these equations into Racket is direct:


@def[
(define (dini p a u0 u1 m v0 v1 n)
  (map-division
    (lambda (u v)
      (+xyz p
            (* (cos u) (sin v))
            (* (sin u) (sin v))
            (+ (cos v) (log (tan (/ v 2.0))) (* a u))))
    u0 u1 m
    v0 v1 n))
]

The following invocations of this function show how it is possible, 
in fact, to simulate the natural shapes of certain types of shells. The 
result is represented in @figref{fig:dini}.


@def/no-results[
(surface-grid
  (dini (xyz 0 0 0) 0.2 0 (* 6 pi) 100 0.1 (* pi 0.5) 100))
(surface-grid
  (dini (xyz 3 0 0) 0.2 0 (* 6 pi) 100 0.1 (* pi 0.7) 100))
(surface-grid
  (dini (xyz 6 0 0) 0.2 0 (* 6 pi) 100 0.1 (* pi 0.9) 100))
]

@figure[#:tag "fig:dini"
        #:caption @elem{The Dini surface as a mathematical approximation of the form of certain types of shells.}]{
@autoimage{conchas}
}

Another example is the spiral shell, defined mathematically 
(in Racket) by:


@def[
(define (shell p a b u0 u1 m v0 v1 n)
  (map-division
    (lambda (u v)
      (let ((e (exp (/ u 6.0 pi)))
            (c (expt (cos (/ v 2.0)) 2)))
        (+xyz p
              (* a (- 1 e) (cos u) c)
              (* b (- e 1) (sin u) c)
              (+ (- 1 (exp (/ u 3.0 pi)) (sin v))
                 (* e (sin v))))))
    u0 u1 m
    v0 v1 n))
]

From the previous function, we can experiment variations like 
the ones presented below, and which generate the surfaces represented 
in @figref{fig:shells}.


@def/no-results[
(surface-grid
  (shell (xyz 0 0 0) 1 1 0 (* 7 pi) 100 0 (* 2 pi) 100))
(surface-grid
  (shell (xyz 7 0 0) 2 2 0 (* 7 pi) 100 0 (* 2 pi) 100))
(surface-grid
  (shell (xyz 18 0 0) 3 1 0 (* 7 pi) 100 0 (* 2 pi) 100))
]

@figure[#:tag "fig:shells"
        #:caption @elem{A mathematical approximation of the shape of spiral shells.}]{
@autoimage{conchas2}
}


@subsection{Cylinders, Cones, and Spheres}

Although we have illustrated the previous section with relatively
complex surfaces, in this section we will start by doing
surface that are comparatively much simpler, which will allow us to realize that the
parametric construction of surfaces is relatively easy as long as we
understand the impact of the variation of the independent parameters
@${u} and @${v}.

For example, the surface of a cylinder of radius @${r} is
characterized by being the set of points that are at a fixed
distance @${r} of an axis and from a minimum @${h_0} and maximum
@${h_1} "height" relative to that axis.

Assuming, for simplicity, that we will match the cylinder axis with
the @${Z} axis, this means that we employ cylindrical coordinates
@${(\rho, \phi,z)}, with the cylinder surface being defined simply by
fixing @${\rho=r} and letting @${\phi} vary between @${0} and
@${2\pi} and @${z} vary between @${h_0} and @${h_1}. With @${\rho} being
fixed and the variations of @${\phi} and @${z} being independent, the use of
parametric surfaces is trivial: we need only use one of the parameters @${u}
or @${v} to make the variation of @${\phi} and the other for the
variation of @${z}.

More specifically, let us make @${\rho(u,v)=r} and let @${\phi(u,v)=u} and
@${z(u,v)=v} vary independently, respectively between @${u\in
  [0,2\pi]} and @${v\in [h_0,h_1]}. Since @${\phi} depends directly
and exclusively on @${u} and @${z} depends directly and exclusively on
@${v}, we can use these parameters directly in the function. This way,
we are able to write a function that produces a cylindrical coordinate
from the value of the radius @${r} and from the independent parameters
@${\phi} and @${z}.

The image on the left in @figref{fig:cilConEsf} shows the cylinder of
radius @${1} between @${h_0=-1} and @${h_1=1}, produced by the
evaluation of following expression:


@def/no-results[
(surface-grid
  (map-division
    (lambda (fi z)
      (cyl 1 fi z))
    0 2pi 60
    -1 +1 20))
]

If, instead of fixing the radius of @${\rho(fi,z)=r}, we make it vary
in function to the height @${z}, then it is obvious that the radius of
the "cylinder" will be zero at the origin and will increase (in absolute
value) as it moves away from the origin. The only difference to the
previous case will thus be on the expression that creates the
cylindrical coordinates, which will now be @lisp[(cyl z fi z)],
reflecting the linear increase of the radius with @${z}. Evidently,
the resulting figure will not be a cylinder but a cone, that we
present in the central image of @figref{fig:cilConEsf}.

Finally, let us consider the description of a sphere of radius @${r}
in spherical coordinates @${(\rho, \phi, \psi)}, which is reduced to
@${\rho=r}, with @${\phi} varying between @${0} and @${2\pi} and
@${\psi} varying between @${0} and @${\pi}. Once again, we need two
independent variations, for which we can arbitrarily assign @${u} to one and
@${v} to the another. Thus, let us make @${\rho(u,v)=r},
@${\phi(u,v)=u} and @${\psi(u,v)=v} with @${u\in [0,2\pi]} and @${v\in
  [0,\pi]}. As is was did in the two previous examples, this
association between @${\phi} and @${\psi} and, respectively, @${u} and
@${v}, allows us to write the function directly in terms of the
former. Exemplifying with @${r=1}, we have:


@def/no-results[
(surface-grid
  (map-division
    (lambda (fi psi)
      (sph 1 fi psi))
    0 2pi 60
    0 pi 30))
]

The result is on the right of @figref{fig:cilConEsf}.

@figure[#:tag "fig:cilConEsf"
        #:caption @elem{A cylinder, a cone and a sphere generated as parametric surfaces.}]{
@autoimage{cilindroConeEsfera}
}

To see the impact that small changes in parameters can have on the
shape of generated surfaces, let us try three variations on the
shape of the sphere. The first variation is to add to the radius of the
sphere a sinusoid with high frequency and low amplitude, in terms of
"latitude" @${\psi}, for example, by making
@${\rho=1+\frac{\sin(20\psi)}{20}}. The result will be a formation of
waves from pole to pole, as is visible in the image on the right of
@figref{fig:esferasOndas}. If, on the other hand, we decide to
make the same change in radius to be dependent on an identical
sinusoid but in terms of @${\phi}, then the waves travel along the
"longitude", for example, from east to west. This effect is illustrated on
the central image of @figref{fig:esferasOndas}. Finally, we can get
a combined effect by making the radius simultaneously dependent on
a sinusoid in terms of "latitude" and other sinusoid in terms of
"longitude", i.e.,
@${\rho=1+\frac{\sin(20\phi)}{20}+\frac{\sin(20\psi)}{20}}. The result
is shown on the right image of @figref{fig:esferasOndas}.

@figure[#:tag "fig:esferasOndas"
        #:caption @elem{Spheres of which the radius is a function of the "latitude", "longitude" or both.}]{
@autoimage{esferasSeno}
}

@questions[
@question{
Consider the following ellipsoid:


@fig{@autoimage[#:scale 0.7]{elipsoide}}

An ellipsoid is characterized by the dimensions of its three 
orthogonal radii @${a}, @${b} and @${c}. Its parametric equation is:

@$$|{\begin{aligned}
  x&=a\sin \psi \cos \phi\\
  y&=b\sin \psi \sin \phi\\
  z&=c\cos \psi\end{aligned}}|
@$${\begin{matrix}-\frac{\pi}{2}\leq\phi\leq+\frac{\pi}{2};\quad-\pi\leq\psi\leq+\pi;\end{matrix}}

Define the @fn[ellipsoid] function that produces the ellipsoid from
the three radii @${a}, @${b}, @${c}, and also from the number @${n} of
values to use along @${\phi} and @${\psi}.  }
]

@section{Bodegas Ysios}

The Bodegas Ysios is a building designed by Santiago Calatrava and
intended for the production, storage and distribution of wines.
@figref{fig:aya} shows a frontal view of the building.


@figure[#:tag "fig:aya"
        #:caption @elem{Bodegas Ysios. Photograph by Luis Antonio Ortuño.}]{
@authorizedPhoto{sinus/LAOMadrid0}
}

The building has two longitudinal walls of sinusoidal shape of which the top
finishing is also sinusoidal. These walls are 196 meters long and are
spaced from one another by 26 meters along the midline. The roof has
the shape of a wave made of parallelepipeds which rest on the
tops of the sinusoidal walls.  Note that the tops of the walls, where
the parallelepiped rest, are generally at different heights,
since the two top sinusoids have opposite phases with respect to
one another. @footnote{The opposing phases means that when one of the
sinusoids reaches its maximum value, the other reaches its minimum
and vice versa. For this to happen, the difference between phases of
the two sinusoids has to be @${\pi}.}

As can be seen in @figref{fig:ayc}, ​​the central part of the
south facade, intended to accommodate the visitor center, is more prominent 
and higher than the rest of the building.

As shown in @figref{fig:ayc}, the roof parallelepipeds are made of
aluminium, in contrast with the cedar wood that is used on the south
facade. On the north facade, concrete is used with some small openings
for illumination. The east and west facades consist of wavy aluminium
plates.


@figure[#:tag "fig:ayc"
        #:caption @elem{Bodegas Ysios. Photograph by Koikile.}]{
@authorizedPhoto{sinus/LAOMadrid1}
}

In this section we will address the modelling of the frontal wall of
the wine cellar. In a first analysis, we can model this wall as a
vertical extrusion of a sinusoid. Unfortunately, although this
operation can properly model the lateral sides of the wall, it can not do
so for  the central part because, in this area, the wall has a more
complex evolution that moves away from the vertical.

A second approach could be the interpolation of regions, but this will
only be achievable if we can shape the curves that delimit the
wall. In addition to that, a large number of curves may be necessary in order
to accurately represent the surface of the wall.

A third approach, more rigorous, consists in the mathematical description of
the surface in question. This way, the required "curves" could be
generated automatically. While it may be difficult to see at first
glance that the wall of Bodegas Ysios can be modelled by a mathematical
function, we will see that it is relatively simple to undertake this
modelling through the composition and modification of simpler
functions.

As a first approximation, we can start by considering a sinusoidal
wall identical to what we would get if we were to make a vertical
extrusion of a sinusoid. For simplicity, we will centre the wall base
at the origin, which implies that the central hump has its maximum
amplitude at the origin. Seeing as the sine amplitude is zero at the
origin, this suggests that instead of a sine, it is preferable we use a
cosine, of which the amplitude is maximum at the origin. The observation of
this work by Calatrava indicates that this cosine performs three cycles for
each side, for which the period should range between @${-6\pi} and
@${+6\pi}. Admitting that this wall evolves along the @${XZ} plane, we
can start by experimenting the following expression:

@def/no-results[
(surface-grid
  (map-division
    (lambda (x z)
      (xyz x
           (cos x)
           z))
    (* -7 pi) (* 7 pi) 100
    0 5 10))
]

The expression shows that we make the @${x} coordinate vary freely
with @${u}, we make the @${y} coordinate be the @${x} cosine, and finally we make the
@${z} coordinate vary freely with @${v}. As we can observe in
@figref{fig:ysiosA}, the surface is a good approximation of the lateral
sides of the front wall of the Bodegas Ysios.

@figure[#:tag "fig:ysiosA"
        #:caption @elem{First approach to modelling the Bodegas Ysios.}]{
  @autoimage{ysiosA}}

Let us now model the sinusoids at the top of the wall, where the roof's
parallelepipeds are supported. For this, we will simply
"deform" the surface, causing the @${z} coordinate to oscillate in a
cos-sinusoidal way along the @${x} axis, increasing the oscillation
amplitude as we go up in @${z}. In order for the wall to have its
base free of the influence of this cos-sinusoid, we will raise the cosine
by one unit and use a reduced initial amplitude:

@def/no-results[
(surface-grid
  (map-division
    (lambda (x z)
      (xyz x
           (cos x)
           (* z (+ 1 (* 0.2 (cos x))))))
    (* -7 pi) (* 7 pi) 100
    0 5 10))
]

The result of the previous modelling is shown in @figref{fig:ysiosB}.

@figure[#:tag "fig:ysiosB"
        #:caption @elem{Second approach to modelling the Bodegas Ysios.}]{
@autoimage{ysiosB}
}

To model the increase in amplitude that the central "hump" will have
with the height, we can consider increasing the cosine amplitude as the
@${z} height increases, by doing:

@def/no-results[
(surface-grid
  (map-division
    (lambda (x z)
      (xyz x
           (* (+ z 3) (cos x))
           (* z (+ 1 (* 0.2 (cos x))))))
    (* -7 pi) (* 7 pi) 100
    0 5 10))
]

The result, seen in @figref{fig:ysiosC}, shows that the evolution
of the central part is already correctly modelled, but it is necessary
to avoid that this change also affects the lateral sides. For this, all we need
to do is a combination of two separate evolutions, one for the
central area of the wall, corresponding to half of a cosine cycle,
i.e., a range of variation of @${x} between @${-\frac{\pi}{2}} and
@${+\frac{\pi}{2}}, and another for everything else. We must take into
account that in order to compensate the slope difference caused by
the increase of the @${y} coordinate in the central area we must also
increase the variation amplitude in @${z}. Thus, we have:

@figure[#:tag "fig:ysiosC"
        #:caption @elem{Third approach to modelling the Bodegas Ysios.}]{
@autoimage{ysiosC}
}

@def/no-results[
(surface-grid
 (map-division
  (lambda (x z)
    (xyz x
         (if (<= -pi/2 x pi/2)
           (* (+ z 3) 0.8 (cos x))
           (cos x))
         (if (<= -pi/2 x pi/2)
           (* z (+ 1 (* 0.4 (cos x))))
           (* z (+ 1 (* 0.2 (cos x)))))))
  (* -7 pi) (* 7 pi) 100
  0 5 10))
]

The result of this change is shown in @figref{fig:ysiosD}.


@figure[#:tag "fig:ysiosD"
        #:caption @elem{Fourth approach to modelling the Bodegas Ysios.}]{
@autoimage{ysiosD}
}

The final format of the function already gives a good approximation to
the shape of the cellar. This function can be seen as the mathematical
representation of Calatrava's ideas for the frontal wall of the Bodegas Ysios.

@questions[
@question{ Define the function @fn[polygonal-prism] that, from a
  list of coplanar points and a displacement @${d_x} parallel to the
  @${x} axis, creates a polygonal prism such as the one shown in the
  following diagram which was generated by @lisp[(polygonal-prism (list
    @${P_0} @${P_1} @${P_2} @${P_3}) @${d_x})]
	
@fig{@tex{
\begin{tikzpicture}[scale=1.4]
\inputtikz{prismaPoligonal}
\end{tikzpicture}
}}
}

@question{ Define a function @fn[roof-ysios] capable of creating roofs
  with the same "style" as the Bodegas Ysios, but where the shape of the roof
  is relatively arbitrary. To allow this arbitrariness, assume that the
  roof is defined only by four lists with the same number of points
  (represented by their coordinates) and where the points that are in the
  same position in all four lists are in the same plane parallel to
  the @${YZ} plane, such as outlined in the following figure:

@fig{@tex{
\begin{tikzpicture}[very thin,scale=0.7]
\draw[-latex](4.242cm,4.377cm)--(5.309cm,4.276cm)node[right]{$x$};
\draw[-latex](4.513cm,4.394cm)--(3.945cm,4.208cm)node[left]{$y$};
\draw[-latex](4.42cm,4.166cm)--(4.42cm,5.351cm)node[above]{$z$};
\inputtikz{ysios}
\end{tikzpicture}
}}

Using one point of each of the four lists, use the function
@fn[polygonal-prism] (explained in the previous exercise) to create
a prism in which the thickness @${d_x} is equal to the distance between the
planes defined by these points and by the next four points. The
previous image shows two prisms constructed by this process.  }

@question{ Define the function @fn[ysios-curve] which, conveniently
  parametrized, is capable of generating a list with one of the vertices of each
  of the successive prisms that constitute a roof in the style of the Bodegas Ysios.
  The following image was generated by the function
  @fn[roof-ysios] using four invocations of the function
  @fn[ysios-curve] as arguments.
	

  @fig[@autoimage{ysiosCobertura}]
}

@question{ Identify a combination of parameters for the functions
  @fn[ysios-walls] and @fn[ysios-curve] that is capable of
  approximately reproducing the facade and the roof of the Bodegas Ysios,
  as presented in the following image.
	
@fig[@autoimage{ysiosFinal}]

}

@question{
The following image depicts a torus:

@fig{@autoimage[#:scale 0.5]{toro}}

Deduce the parametric equation of the torus and define the Racket function
@fn[torus] that creates a torus similar to one on the previous image from
the centre @${P} of the torus, the greater @${r_0} and smaller @${r_1}
radii and the number of intervals @${m} and @${n} to consider in each
dimension.

}

@question{ Redefine the previous function to generate a spheres torus
  similar to the one shown in the following image:

@fig{@autoimage[#:scale 0.5]{toroEsferas}}

}

@question{
The "apple" presented below can be described by the parametric equation: 
	
@$$|{\begin{aligned}
  x&=\cos u \cdot (4 + 3.8  \cos v)\\
  y&=\sin u \cdot (4 + 3.8  \cos v)\\
  z&=(\cos v + \sin v - 1) \cdot (1 + \sin v) \cdot \log(1 - \pi\cdot\frac{v}{10}) + 7.5 \sin v\end{aligned}}|
@$${\begin{matrix}0\leq u\leq 2\pi;\quad-\pi\leq v\leq\pi;\end{matrix}}

@fig[@autoimage[#:scale 0.5]{apple}]

Write a Racket expression that reproduces the previous "apple".
}

@question{
Consider the "rose" presented below:
	
  @fig[@autoimage[#:scale 0.7]{rose}]
  
  This "rose" was generated from a parametric equation discovered by
  Paul Nylander. Search for this equation and write a Racket
  expression that can generate the corresponding surface.  }
]

@section{Surface Normals}

When a surface is not planar, but is @emph{almost}-planar, it can be difficult
for an observer to perceive its curvature. In such cases, it is very useful to
have a mechanism that allows us to visualize the effects of this curvature
without altering the shape of the surface. The superimposition of normal
vectors is a simple process to do this visualization, as can be seen in the
images presented in @figref{fig:superficieNormais}.  On the left, we see a
surface where the curvature is not perfectly obvious. On the right, we see the
same surface but with the normal vectors clearly showing the curvature.

@figure[#:tag "fig:superficieNormais"
        #:caption @elem{The image on the left shows an almost flat surface. The image on the right shows the same surface with overlapping normal vectors, allowing a better understanding of its curvature.}]{
    @autoimage[#:scale 0.49]{superficieSemNormais}
    @hfill{}
    @autoimage[#:scale 0.49]{superficieNormais}}

To overlap the normal vectors we have to calculate their position and
direction. Assuming that the surface is discretized in a mesh
(as produced, for example, by the function @fn[map-division]), that
mesh is composed by a succession of quadrangles that extend along two
directions. To see the curvature, we can position each vector at the
centre of each of these quadrangles, according to that quadrangle's normal
direction.

@figure[#:tag "fig:centroideQuadrangulo"
        #:caption @elem{The centre of a quadrangle as the average midpoint of the diagonal midpoints.}]{
@tex{
  \begin{tikzpicture}[scale=0.7]
    \inputtikz{centroideQuadrangulo}
  \end{tikzpicture}
}}

To calculate the centre of a quadrangle we can employ the approach 
shown in @figref{fig:centroideQuadrangulo}. The centre of the 
quadrangle defined by the points @${P_0}, @${P_1}, @${P_2} and @${P_3} can 
be obtained by determining the midpoints @${M_{02}} and @${M_{13}} of 
the quadrangle's diagonals and, finally, the midpoint @${M} of this
segment. Translating this into Racket we simply have:

@def[
(define (centre-quadrangle p0 p1 p2 p3)
  (midpoints
    (midpoints p0 p2)
    (midpoints p1 p3)))
]

@figure[#:tag "fig:normalQuadrangulo"
        #:caption @elem{Normal at the centre of a quadrangle (planar on the left and non-planar on the right).}]{
@tex{
\begin{tikzpicture}[scale=1.5]
  \inputtikz{normalQuadranguloA}
\end{tikzpicture}
@hfill{}
\begin{tikzpicture}[scale=1.5]
  \inputtikz{normalQuadranguloB}
\end{tikzpicture}
}}

The calculation of the normal of a quadrangle is trivial when the
quadrangle is planar, as it is enough to perform the cross product of
two edges that meet at the same vertex, as we can see on the
left of @figref{fig:normalQuadrangulo}. However, in the case of a
non-planar quadrangle (on the right of @figref{fig:normalQuadrangulo}),
that logic can i no longer applicable, because there may be different
normals at each vertex. It is however possible to compute an
approximate normal using Newell's method @cite{Sutherland et al 1974}
which is based on the fact that the projections of the areas
of a polygon onto the Cartesian planes are proportional to the
components of the normal vector of that polygon. Newell's method
calculates these areas and, from them, derives the normal vector
@${\vec{N}}. Mathematically speaking, given a polygon with @${n}
vertices @${v_0,v_1,\ldots,v_n}, the method consists in calculating
the components of the normal vector @${\vec{N}} through the sum of areas:

@$${N_x=\sum_{i=0}^{n}(v_{i_y}-v_{{i+1}_y})\cdot(v_{i_z}+v_{{i+1}_z})}
@$${N_y=\sum_{i=0}^{n}(v_{i_z}-v_{{i+1}_z})\cdot(v_{i_x}+v_{{i+1}_x})}
@$${N_z=\sum_{i=0}^{n}(v_{i_x}-v_{{i+1}_x})\cdot(v_{i_y}+v_{{i+1}_y})}

To transform the resulting vector into a unit vector, we simply have to
@emph{normalize} it, i.e., divide each component by its 
length. Translated into Racket, we have:


@def[
(define (polygon-normal pts)
  (normalized-vector
   (cross-products
    (append pts (list (car pts))))))
]

@def[
(define (cross-products pts)
  (if (null? (cdr pts))
    (xyz 0 0 0)
    (+c (cross-product (car pts) (cadr pts))
        (cross-products (cdr pts)))))
]

@def[
(define (cross-product p0 p1)
  (xyz (* (- (cy p0) (cy p1)) (+ (cz p0) (cz p1)))
       (* (- (cz p0) (cz p1)) (+ (cx p0) (cx p1)))
       (* (- (cx p0) (cx p1)) (+ (cy p0) (cy p1)))))
]

@def[
(define (normalized-vector v)
  (let ((l (sqrt (+ (sqr (cx v))
                    (sqr (cy v))
                    (sqr (cz v))))))
    (xyz (/ (cx v) l)
         (/ (cy v) l)
         (/ (cz v) l))))
]

In the case of a quadrangle, we can simply define

@def[
(define (quadrangle-normal p0 p1 p2 p3)
  (polygon-normal (list p0 p1 p2 p3)))
]

Truth be told, some of the operations previously outlined are predefined in
Rosetta, particularly, the cross product (called @fn[cross-c]), and
the normalized vector (called @fn[norm-c]).

To visually represent the normal we are going to use a very thin
cylinder of which the size will be proportional to the size of the
quadrangle, so that it automatically adapts to different
quadrangles. Naturally, it is also possible to employ other approaches,
for example, adopting fixed dimensions.


@def[
(define (normal pt0 pt1 pt2 pt3)
  (let ((c (quadrangle-centre pt0 pt1 pt2 pt3))
        (n (quadrangle-normal pt0 pt1 pt2 pt3))
        (d (distance pt0 pt2)))
    (cylinder c
              (/ d 40)
              (+c c (*c n (* d 1.5))))))
]

The next step is to go through all the mesh quadrangles and construct the
normal in each one. For this, we do:

@def[
(define (normals ptss)
  (for/list ((pts0 ptss)
             (pts1 (cdr ptss)))
    (for/list ((p0 pts0)
               (p1 pts1)
               (p2 (cdr pts1))
               (p3 (cdr pts0)))
      (normal p0 p1 p2 p3))))
]

Even though the visualization of the normals is complementary to the
visualization of the surface, it does not make much sense to see the first without
also viewing the second. Therefore, we will define a function that combines the
two views:

@def[
(define (surface-and-normals ptss)
  (normals ptss)
  (surface-grid ptss))
]

To test this function we can now try to apply it to the generation of normals
on the Möbius strip:

@def[
(surface-and-normals (mobius-strip 0 4pi 160 0 0.3 5))
]

@Figref{fig:normaisMoebius} shows the result.


@figure[#:tag "fig:normaisMoebius"
        #:caption @elem{The Möbius surface with the superimposed normals.}]{
@autoimage{moebiusNormais}
}

@section{Surface Processing}

A careful observation of the functions @fn[normal-curves] and
@fn[normal-strip-curves] shows that they iterate along the surface
quadrangles, computing the normal at each one. Naturally, we can
generalize this process so that it is possible to process a surface by
applying any operation to all its quadrangles. The transition to a
higher-order function is trivial:


@def[
(define (iterate-quadrangles f ptss)
  (for/list ((pts0 ptss)
             (pts1 (cdr ptss)))
    (for/list ((p0 pts0)
               (p1 pts1)
               (p2 (cdr pts1))
               (p3 (cdr pts0)))
      (f p0 p1 p2 p3))))
]

From now on, the computation of the normals to a surface can be treated as a
mere particularization of the @fn[iterate-quadrangles] function:

@def[
(define (normals ptss)
  (iterate-quadrangles normal ptss))
]

The great advantage, however, lies in the fact that we can now create many
other ways of processing a surface. As an example, let us consider the
creation of a net, as shown in @figref{fig:superficieCruzes}.
For this example, the shape of the net is defined by the following parametric surface:


@$$|{\left\{
  \begin{aligned}
x(u,v)& = u\\
y(u,v)& = v\\
z(u,v)& = \frac{7}{100}(\sin(u)+\sin(2(v-2)))
  \end{aligned}\right.}|
@$${0\leq u \leq 5}
@$${0\leq v \leq 5}

@figure[#:tag "fig:superficieCruzes"
        #:caption @elem{A net made from crossed cylinders.}]{
@autoimage{superficieCruzes}
}

The creation of the previous net can be done by simply creating a
cross along the quadrangles of a surface. The cross can be modelled by
using two cylinders of which the ends coincide with the vertices of the
diagonals of the quadrangle. For the cylinders to adapt to different
quadrangles, we can consider that the radius of the cylinders is
proportional to the size of the quadrangle. The following function
implements this approach:


@def[
(define (cylinders-cross-quad p0 p1 p2 p3)
  (let ((r (/ (min (distance p0 p1) (distance p0 p3)) 10)))
    (cylinder p0 r p2)
    (cylinder p1 r p3)))
]
To create the net of @figref{fig:superficieCruzes} we just need to do:

@def[
(iterate-quadrangles
 cylinders-cross-quad
 (map-division (lambda (i j)
                 (xyz i j (* 0.07 (+ (sin i) (sin (* 2 (- j 2)))))))
               0 5 20
               0 5 20))
]

The function @fn[iterates-quadrangles] can therefore be used either for 
different surfaces or for different functions to iterate over the 
coordinates of those surfaces.  @Figref{fig:superficieItera} 
shows different constructions performed over the surface defined 
by the following parametric function:

@$$|{\left\{
  \begin{aligned}
x(u,v)& = u\\
y(u,v)& = v\\
z(u,v)& = \frac{4}{10}(\sin(u+v)+e^{\frac{\left|u-1\right|}{10}}+\sin(v-1))
  \end{aligned}\right.}|
@$${0\leq u \leq 3}
@$${0\leq v \leq 6}

@figure[#:tag "fig:superficieItera"
        #:caption @elem{Different constructions made ​​from a surface. From left to right and top to bottom, the repeating element is: (1) four cylinders connected to a sphere; (2) a cube; (3) a cone; (4) a spherical cap; (5) a cylinder; (6) a parallelepiped of random height.}]{
  @autoimage[#:scale 0.8]{superficieEsferaCruz}
  @autoimage[#:scale 0.8]{superficieCubos}
  @autoimage[#:scale 0.8]{superficieCones}
  @autoimage[#:scale 0.8]{superficieCalotas}
  @autoimage[#:scale 0.8]{superficieCilindrosBase}
  @autoimage[#:scale 0.8]{superficieCubos2}
}

In @figref{fig:sinuxvItera} we show the result of the very same 
functions but now iterated over another surface defined by:

@$$|{\left\{
  \begin{aligned}
x(u,v)& = u\\
y(u,v)& = v\\
z(u,v)& = \frac{4}{10}\sin(u\cdot v)
  \end{aligned}\right.}|
@$${-\pi\leq u \leq \pi}
@$${-\pi\leq v \leq \pi}}

@figure[#:tag "fig:sinuxvItera"
        #:caption @elem{Different constructions made ​​from a surface.}]{
  @autoimage[#:scale 0.8]{sinuxvEsferaCruz}
  @autoimage[#:scale 0.8]{sinuxvCubos}
  @autoimage[#:scale 0.8]{sinuxvCones}
  @autoimage[#:scale 0.8]{sinuxvCalotas}
  @autoimage[#:scale 0.8]{sinuxvCilindrosBase}
  @autoimage[#:scale 0.8]{sinuxvCubos2}
}

@questions[
@question{ For each of the examples shown in @figref{fig:superficieItera}
(or, equivalently, in @figref{fig:sinuxvItera}), define the function that receives four points
  as arguments and that, when iterated by the function
  @fn[iterates-quadrangle] along the surface coordinates,
  reproduces the model.  }

@question{ Define a function that, given the coordinates of the four
  vertices of a quadrangle, creates two interlaced cylinders along
  the diagonals of the quadrangle, as presented in the following
  image:
	
@fig{@autoimage[#:scale 0.5]{cilindrosEntrelacados}}

  Use the function that you created as an argument of @fn[iterates-quadrangle]
  in order to generate the "fabric" that is presented below:

  @fig[@autoimage{superficieTecido}]
}

@question{
The approach used in the previous exercise is not adequate when 
the surface in question is not almost-planar because the  
corresponding cylinders corresponding to two adjacent quadrangles might not be 
aligned properly, case in which overlapping will occur or, worse yet, empty spaces.

Redefine the process of creating the fabric so that interlaced
cylinders are used, but where each cylinder corresponds a complete wire
(and not just the section contained in a quadrangle). To simplify,
we can assume that the wires develop along the lines and columns of
the coordinate matrix of the surface, so as to generate images as the
following:

  @fig[@autoimage{superficieTecido2}]

@def/no-show[
(define (oscila-curvas ptss r)
  (cons
   (oscila-faixa-curvas (car ptss) (cadr ptss) r)
   (if (null? (cddr ptss))
    (list)
    (oscila-curvas (cdr ptss) (- r)))))

(define (oscila-faixa-curvas pts0 pts1 r)
  (cons (oscila-centro-quadrangulo (car pts0) (car pts1) (cadr pts1) (cadr pts0) r)
        (if (null? (cddr pts0))
          (list)
          (oscila-faixa-curvas (cdr pts0) (cdr pts1) (- r)))))

(define (oscila-centro-quadrangulo p0 p1 p2 p3 r)
  (+c (centro-quadrangulo p0 p1 p2 p3)
      (*c (normal-quadrangulo p0 p1 p2 p3)
          r)))

(define (matriz-transposta matriz)
  (if (null? (car matriz))
    (list)
    (cons (map car matriz)
          (matriz-transposta (map cdr matriz)))))

(define (tecido malha)
  (let ((p0 (caar malha))
        (p1 (caadr malha))
        (p2 (cadadr malha))
        (p3 (cadar malha)))
    (let* ((d (min (distance p0 p1) (distance p0 p3)))
           (r (/ d 6.0)))
      (map (lambda (curva)
             (sweep (spline curva)
                    (surface (circle (xyz 0 0 0) r))))
           (append (oscila-curvas malha r)
                   (oscila-curvas (matriz-transposta malha) r))))))
]
}]

We have seen in several previous examples a separation between the
generation of a surface's coordinates (using, for example, the
@fn[map-division] function), and the use of these coordinates, for example, for
visualization (using the @fn[superface-grid] function) or for
processing (using the @fn[iterates-quadrangles]).

This separation is advantageous because it allows us to arbitrarily combine
different functions for generating coordinates with
different functions that use those coordinates. Another interesting
possibility is the use of functions that process the coordinates to
produce new coordinates. A trivial example would be the application of
a scale effect over a surface, simply by multiplying the scale by each
of the coordinates, thereby generating a new set of coordinates.

A slightly less trivial example, but far more interesting, is the
creation of trusses of which the shape follows a surface. As we have seen in
section @ref{sec:trelicas}, these trusses are composed of spheres
joined by beams to form quadrangular pyramids. To build a truss that
follows a surface we can simply build each of the truss' pyramids with their
base set on the surface and their top located on the
normal vector to that surface. The function @fn[spatial-truss]
(that we developed in section @ref{sec:trelicaEspacial}) is perfectly
capable of building these trusses, as long as it receives the
positions of the truss' knots in the form of a list with an odd number of
coordinates lists, according to the sequence base-knots, top-knots,
base-knots, top-knots, @ldots, base-knots.

Bearing these assumptions in mind, to build a truss that follows a
surface we can start by generating the coordinates of that surface, but
conveniently spaced so as to serve as positions to the base knots of
each truss pyramid. Next, for each quadrangle of coordinates, we have to
find the pyramid's top knot of which the base is this quadrangle. For
this, we can assume that the trusses will use, whenever possible,
beams with the same dimension @${l}. This implies that the
top knot of each pyramid is located on the normal to the
quadrangle defined by the base knots, at a distance from the centre of
that quadrangle given by @${h=\frac{l}{\sqrt{2}}}. Therefore, we have
to calculate both the centre and the quadrangle's normal. The
following function illustrates this process:

@def[
(define (vertex-quadrangular-pyramid p0 p1 p2 p3)
  (let ((h (/ (+ (distance p0 p1)
                 (distance p1 p2)
                 (distance p2 p3)
                 (distance p3 p0))
              4.0
              (sqrt 2))))
    (+c (quadrangle-centre p0 p1 p2 p3)
        (*c (quadrangle-normal p0 p1 p2 p3)
            h))))
]

The next step will be to use this function to compute the vertices'
coordinates along the surface mesh coordinates, creating new sequences
of coordinates that are insert between the sequences of coordinates
of the original mesh, in order to reproduce the alternation between
base-knots and top-knots of the truss.

The two following functions perform this task:


@def[
(define (insert-vertex-pyramid ptss)
  (if (null? (cdr ptss))
    ptss
    (cons
     (car ptss)
     (cons (insert-vertex-pyramid-2 (car ptss) (cadr ptss))
           (insert-vertex-pyramid (cdr ptss))))))
]

@def[
(define (insert-vertex-pyramid-2 pts0 pts1)
  (cons (vertex-quadrangular-pyramid (car pts0) (car pts1) (cadr pts1) (cadr pts0))
        (if (null? (cddr pts0))
          (list)
          (insert-vertex-pyramid-2 (cdr pts0) (cdr pts1)))))
]

Finally, given any mesh, we simply have to chain the creation of vertices with 
the creating of the truss, i.e.:

@def[
(space-truss
  (insert-vertex-pyramid
    mesh))
]

@figure[#:tag "fig:superficieTrelica"
        #:caption @elem{Truss built over a surface.}]{
@autoimage{superficieTrelica}
}

@Figref{fig:superficieTrelica} shows the result of creating a
truss over the same surface used in @figref{fig:superficieItera}.
Naturally, we can combine the truss construction with any other surface.
@Figref{fig:sinuxvTrelica} shows a truss built over the same surface
used in @figref{fig:sinuxvItera}.


@figure[#:tag "fig:sinuxvTrelica"
        #:caption @elem{Truss built over a surface.}]{
@autoimage{sinuxvTrelica}
}

Finally, out of curiosity, @figref{fig:moebiusTrelica} 
shows a truss built over a Möbius strip. This truss 
was produced by the evaluation of the following expression:

@def[
(space-truss
  (insert-vertex-pyramid
    (mobius-strip2 0 4pi 160 0 0.3 5)))
]

@figure[#:tag "fig:moebiusTrelica"
        #:caption @elem{A truss with the shape of a Möbius strip.}]{
@autoimage{moebiusTrelica}p 
}

@questions[
@question{
If you look closely you will notice the truss has a "default". 
Identify it and explain it.
}

@question{
Consider the "hedgehog" and "cactus" shown in the following image:

@fig{
   @hfill{}@autoimage[#:scale 0.45]{ourico}@hfill{}@autoimage[#:scale 0.45]{cacto}@hfill{}
}

Define a function that, conveniently parametrized, is capable of
creating "hedgehogs" and "cacti" as the previous ones.

@def/no-show[
(define (pontos-ourico p r a h f n)
  (map-division
    (lambda (fi psi)
      (+sph p
            (* (+ r (* a (sin (* f fi)) (sin psi))) (+ h (sin psi)))
            fi
            psi))
    0 2pi n
    0 (* 0.7 pi) (/ n 2)))

(define (agulha-quad p0 p1 p2 p3)
  (define p (centro-quadrangulo p0 p1 p2 p3))
  (define n (normal-quadrangulo p0 p1 p2 p3))
  (cone p
        0.2
        (+c p (*c n -5))))

(define (ourico p r a h f n n-agulhas)
  (surface-grid (pontos-ourico p r a h f n))
  (with-current-layer
   "Black"
   (itera-quadrangulos agulha-quad (pontos-ourico p r a h f n-agulhas))))
]
}
]