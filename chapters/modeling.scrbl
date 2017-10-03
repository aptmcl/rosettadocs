#lang scribble/manual
@(require "../rosetta.rkt")
@setup-math

@defmodule[rosetta/tikz]

@title{Modelling}

We saw in previous sections some types of pre-defined data in Racket.
In many cases these are enough to build our programs. But in other cases
it will become necessary to introduce new types of data. In this section
we will study a new type of data that will become particularly useful to
model geometric entities: coordinates. 


@section{Coordinates}

Architecture relies on positioning elements in space. That position is
expressed in terms of of what we designate as @emph{Coordinates}: each
coordinate is a number and a sequence of coordinate identifies a point in
space. @figref{fig:coordstresd} demonstrates one possible sequence of
coordinates @${(x,y,z)} that identify a point @${p} in a three-dimensional
space. Different types of coordinate systems are possible and, in the case of
@figref{fig:coordstresd}, we are using a system called @${rectangular}, also
known as @${Cartesian} in honour of its inventor: René
Descartes. @footnote{Descartes was a 19th Century French philosopher, author of
the famous quote: "Cogito ergo sum", (I think, therefore I am) and of countless
contributions in the fields of Mathematics and Physics.}

@figure[#:tag "fig:coordstresd"
        #:caption @elem{Cartesian coordinates of a point in space.}]{
  @tex{
  \tdplotsetmaincoords{55}{35}
  \begin{tikzpicture}[tdplot_main_coords,scale=0.7]
    \inputtikz{coordstresd}
  \end{tikzpicture}
}}

There are other types of useful operations that we can make using
coordinates. For example, we can calculate the distance between two
positions in space @${P_0=(x_0,y_0,z_0)} and @${P_1=(x_1,y_1,z_1)}.
That distance can be expressed using the formula:

@$${d = \sqrt{(x_1-x_0)^2+(y_1-y_0)^2+(z_1-z_0)^2}}

and a first draft of this definition in Racket would be:

@def[
(define (dist x0 y0 z0 x1 y1 z1)
  (sqrt (+ (sqr (- x1 x0))
           (sqr (- y1 y0))
           (sqr (- z1 z0)))))]

The distance between @${(2,1,3)} and @${(5,6,4)} would then be:

@incremental[(dist 2 1 3 5 6 4)]

Unfortunately, by treating coordinates as a set of three independent
numbers the use of functions becomes unclear. This can already be seen
in the previous example, where the function @lisp[dist] calls upon
six parameters forcing the reader to know where the coordinates of one
point start and where they end. This problem becomes even worse when
a function must return, not a number as it so happens with the function
@lisp[dist], but rather a position in space, as it happens, for example,
with the function that computes the mid position @${P_m} between
@${P_0=(x_0,y_0,z_0)} and @${P_1=(x_1,y_1,z_1)}. That position can be
calculated using the formula:

@$${P_m=(\frac{x_0+x_1}{2},\frac{y_0+y_1}{2},\frac{z_0+z_1}{2})}

But it is difficult to conceive a function that implements this because,
apparently, it would have to calculate three different result simultaneously, 
one for each of the coordinates @${X}, @${Y} and @${Z}.

To deal with this kind of problems, mathematicians came up with the concept
of @${tuple}: a tuple is nothing more than group of values but, in certain cases,
this grouping has a specific name. For example, a rational number is tuple
since it groups a number and a denominator, the same way that a position in
space is also a tuple since it groups three coordinates.

All programming languages have mechanisms to create and manipulate
tuples. In Racket, in addition to the predefined tuples, like rational
and complex numbers, various other tuples have already been defined
in the module @lisp[rosetta] which will be very useful to us when modelling
geometry.

To use them, we must first require the module @lisp[rosetta]

@verbatim{
#lang racket
(require (planet aml/rosetta))
}

To set the coordinates @${(x,y,z)}, Rosetta provides the function @fn[xyz]:

@incremental[(xyz 1 2 3) (xyz (* 2 3) (+ 4 1) (- 6 2))]

Note that the result from evaluating the expression @lisp[(xyz 1 2 3)]
is a value that represents a position in the three-dimensional
Cartesian space. That value is expressed as @tt{#<xyz:@italic{x} @italic{y} @italic{z}>}
in which @tt{@italic{x}}, @tt{@italic{y}}, and @tt{@italic{z}} are the coordinate values.

@section{Operations with Coordinates}

Now that we know how to create coordinates, we can rethink the functions
that manipulate them. Let us begin with the function that calculates the
distance between two points @${P_0=(x_0,y_0,z_0)} and @${P_1=(x_1,y_1,z_1)},
which, as we saw, can be calculated using the formula:

@$${\sqrt{(x_1-x_0)^2+(y_1-y_0)^2+(z_1-z_0)^2}}

A first draft of this definition translated into Racket would be:

@lispcode[
(define (dist p0 p1)
  (sqrt (+ (sqr (- ? ?))
           (sqr (- ? ?))
           (sqr (- ? ?)))))]

In order to complete the function we need to know how to get the @${x},
@${y}, and @${z} coordinates of a certain position @${P}. For that
purpose, Rosetta provides the functions @fn[cx], @fn[cy] and @fn[cz],
abreviations for @tt{c}@emph{coordinate} @tt{x}, @tt{c}@emph{coordinate} @tt{y}
and @tt{c}@emph{coordinate} @tt{z} respectively.

Using these functions we can now write:

@def[
(define (dist p0 p1)
  (sqrt (+ (sqr (- (cx p1) (cx p0)))
           (sqr (- (cy p1) (cy p0)))
           (sqr (- (cz p1) (cz p0))))))]

We can test is using a specific case:@footnote{The @lisp[dist] is pre-defined in Rosetta under the name @fn[distance].}

@incremental[(dist (xyz 2 1 3) (xyz 5 6 4))]

Let us look at another example. Suppose we wanted to define a function that
calculates the position of a point after a translation, expressed in terms
of its orthogonal components @${\Delta_x}, @${\Delta_y} and @${\Delta_z}, as
can be seen in @figref{fig:translation}. For @${P=(x,y,z)} we will have 
@${P'=(x+\Delta_x,y+\Delta_y,z+\Delta_z)}. To make the use of this function
easier we shall call it @fn[+xyz]. Naturally, it needs as inputs a starting point
@${P} and the increments @${\Delta_x}, @${\Delta_y} and @${\Delta_z} that we will
call @lisp[dx], @lisp[dy], and @lisp[dz], respectively.

@figure[#:tag "fig:translation"
        #:caption @elem{The point @${P'} as a result of the translation of  
    point @${P=(x,y,z)} of @${\Delta_x} in the @${X} axis, of the @${\Delta_y} in 
    @${Y} axis and @${\Delta_z} in the @${Z} axis.}]{
  @tex{
  \tdplotsetmaincoords{55}{35}
  \begin{tikzpicture}[tdplot_main_coords,scale=0.7]
    \inputtikz{deslocamento}
  \end{tikzpicture}
}}

The definition of this function is as follows:

@lispcode[
(define (+xyz p dx dy dz)
  (xyz (+ (cx p) dx)
       (+ (cy p) dy)
       (+ (cz p) dz)))]

Naturally we ca now use @fn[+xyz] to define new functions, as for example, the horizontal
and vertical translation:

@lispcode[
(define (+x p dx)
  (+xyz p dx 0 0))

(define (+y p dy)
  (+xyz p 0 dy 0))

(define (+z p dz)
  (+xyz p 0 0 dz))]

Equally useful would be the functions that compute the diagonal translations
through the orthogonal planes @emph{XY}, @emph{XZ} and @emph{YZ}:

@lispcode[
(define (+xy p dx dy)
  (+xyz p dx dy 0))

(define (+xz p dx dz)
  (+xyz p dx 0 dz))

(define (+yz p dy dz)
  (+xyz p 0 dy dz))]

The effect of this function can be seen in @figref{fig:efeitotrans}.

@figure[#:tag "fig:efeitotrans"
        #:caption @elem{Translations made by @lisp[+x],
    @lisp[+y], @lisp[+z], @lisp[+xy], @lisp[+xz], @lisp[+yz] and
    @lisp[+xyz] from an arbitrary @${P} point and the by 
    @${\Delta_x}, @${\Delta_y} and @${\Delta_z}.}]{
  @tex{
  \tdplotsetmaincoords{55}{35}
  \begin{tikzpicture}[tdplot_main_coords,scale=1.1]
    \inputtikz{efeitotrans}
  \end{tikzpicture}
}}

Note that every function defined in this section is based on the
@fn[xyz], @fn[cx], @fn[cy] and @fn[cz] operations, which we
can consider to be the fundamental operations on coordinates. The
first one allows us to construct a position given three numbers and
the others to know which numbers determine a position. For this reason,
the first operation is said to be a @emph{constructor} of coordinates
and the others @emph{selectors} of coordinates.

Although we are unaware on how these functions operate internally, we
know they are consistent with each other, ensured by the following expressions:

@lisp[(cx (xyz _x _y _z))] @${=} @lisp[_x]

@lisp[(cy (xyz _x _y _z))] @${=} @lisp[_y]

@lisp[(cz (xyz _x _y _z))] @${=} @lisp[_z]

@questions[
@question{Define the function @fn[midpoint] that calculates the
three-dimensional coordinates of the midpoint between two points @${P_0} and
@${P_1}, given their three-dimensional coordinates.}

@question[#:tag "exe:equalc"]{Define the function @fn[=c] that compares two points
coordinates and returns true if they are the coincident. Note that two points are
coincident when their @${x}, @${y}, and @${z} coordinates are equal.}
]

@def/no-show[
(define (=c p0 p1)
  (and (= (cx p0) (cx p1))
       (= (cy p0) (cy p1))
       (= (cz p0) (cz p1))))
]

@subsection{Bi-dimensional Coordinates}

Just as three-dimensional coordinates locate points in space, bi-dimensional
coordinates locate points in a plane. The question asked is: which plane?  From a
mathematical point of view, this question is not relevant since it is perfectly
possible to think about geometry in a plane without needing to visualize the
plane itself. But when we try to visualize that geometry in a 3D modelling program
we must inevitably think where that plane is located. If omitted, CAD applications
will by default consider the bi-dimensional plane to be the @${XY} plane with the
height @${Z} being zero. So let us consider the bi-dimensional coordinate @${(x,y)}
as a simplified notation for the three-dimensional coordinate @${(x,y,0)}.

Based on this simplification, we can define a bi-dimensional coordinates
constructor in terms of the three-dimensional coordinates constructor:

@lispcode[
(define (xy x y)
  (xyz x y 0))
]

One of the advantages of defining bi-dimensional coordinates as a
particular case of three-dimensional coordinates is that the selectors
@fn[cx], @fn[cy] and @fn[cz] are automatically applicable to bi-dimensional
as well, as are the other functions we created, such as @fn[distance] and
the translations - @fn[+x], @fn[+y], @fn[+z], @fn[+xy], @fn[+xz], @fn[+yz]
and @fn[+xyz].

@questions[
@question{Given the point @${P_0=(x_0,y_0)} and a line defined by two
points @${P_1=(x_1,y_1)} and @${P_2=(x_2,y_2)}, the minimum distance @${d}
between @${P_0} and the line is given by:

@$${d=\frac{\left\vert(x_2-x_1)(y_1-y_0)-(x_1-x_0)(y_2-y_1)\right\vert}{\sqrt{(x_2-x_1)^2+(y_2-y_1)^2}}}

Define a function @fn[point-line-distance] that given the coordinates
of @${P_0}, @${P_1} and @${P_2} returns the minimum distance between
@${P_0} and the line defined by @${P_1} and @${P_2}.
}


@question{Knowing that the maximum step height allowed for each step is @${0.18}m
define a function that calculates the minimum number of steps needed for a flight
of stairs, shown in the following diagram, to connect @${P_1} to @${P_2}.
@fig{
  @tex{
    \begin{tikzpicture}
      \inputtikz{minimoEspelhos}
    \end{tikzpicture}}
}}
]

@subsection{Polar Coordinates}  

Although the Cartesian coordinate system is largely used, there are
other coordinate systems that can be more useful in certain situations.
As an example, suppose we wanted to set @${n} elements, equally spaced
between them and with distance @${d} from the origin point, as is partially
represented in @figref{fig:posicoesPolares}. Logically, the elements will
eventually form a circle and it is easy to see that the angle between them
would have to be @${\frac{2\pi}{n}}.

@figure[#:tag "fig:posicoesPolares"
        #:caption @elem{Positions along a circle.}]{
  @tex{
  \begin{tikzpicture}[scale=4]
    \inputtikz{posicoesPolares}
  \end{tikzpicture}
}}

Taking the @${X} axis as reference, we can say that the first element
will be positioned at @${d} distance from the origin point, the
second element will have the same distance but on a different axis 
that makes a @${\frac{2\pi}{n}} angle with the @${X} axis. The third element will
have the same distance but in a different axis, making an angle of @${\frac{2\pi}{n}}
wit the @${X} axis, and so on. However, when trying to define those positions
using Cartesian coordinates we would find that the regularity expressed with
the "and so on" is immediately lost. This should make us consider a different
system of coordinates, namely the polar coordinate system.

As represented in @figref{fig:coord}, a position in a bi-dimensional plane
is expressed, in rectangular coordinates, by the numbers @${x}, and @${y} -
respectively the @emph{x-axis} and @emph{y-axis}, while in polar coordinates
it is expressed by @${\rho} and @${\phi} - respectively the radius vector
(also called @emph{module}) and the polar angle (also called @emph{argument}).
With the help of trigonometry and the Pythagorean theorem it is easy to convert
polar coordinates into rectangular coordinates:

@$$|{\left\{\begin{aligned}
x&=\rho \cos \phi\\
y&=\rho \sin \phi
\end{aligned}\right.}|

or from rectangular coordinates to polar coordinates:

@$$|{\left\{\begin{aligned}
\rho&=\sqrt{x^2 + y^2}\\
\phi&=\arctan \frac{y}{x}
\end{aligned}\right.}|

@figure[#:tag "fig:coord"
        #:caption @elem{Rectangular and polar coordinates.}]{
  @tex{
  \begin{tikzpicture}[scale=4]
    \inputtikz{coord}
  \end{tikzpicture}
}}

Based on the above equations we can define the constructor of polar coordinates
@fn[pol] (abbreviation of "@bold{pol}ar") that will construct coordinates from
its polar representation, by simply converting them into the equivalent rectangular
ones:

@lispcode[
(define (pol ro fi)
  (xy (* ro (cos fi))
      (* ro (sin fi))))
]

That being said, polar coordinate will be implemented based on the
rectangular system. For that reason, the polar coordinates selectors -
the function (@fn[pol-rho] that allows us to obtain the @${\rho} value
and the function @fn[pol-phi] that allows us to obtain the @${\phi} value)
- must use the rectangular coordinates selectors, i.e., @fn[cx] and @fn[cy].
@footnote{These functions are already predefined in Rosetta.}

@lispcode[
(define (pol-rho c)
  (sqrt (+ (sqr (cx c)) (sqr (cy c)))))

(define (pol-phi  c)
  (atan (cy c) (cx c)))
]

Here are some examples of these functions: @footnote{Note that in some cases the coordinates
numbers are not zero or one as we would expect, but values very close to them. This is due to
rounding errors. And also note that since we are using bi-dimensional coordinates the @${z}
coordinate is always zero.}

@incremental[
(pol 1 0)
(pol (sqrt 2) (/ pi 4))
(pol 1 (/ pi 2))
(pol 1 pi)
]

Another very useful operation is the one that, given a point @${P=(x,y)}
and a ""vector"" with its origin in @${P} and specified in polar coordinates
by a distance @${\rho} and an angle @${\phi}, returns the the point located
at the end of the vector, as shown in @figref{fig:somaCoordsPol}. Using
trigonometry it is easy to calculate that this position given by @${P'=(x+\rho\cos\phi, y+\rho\sin\phi)}.

@figure[#:tag "fig:somaCoordsPol"
        #:caption @elem{A point displacement in polar coordinates}]{
  @tex{
  \begin{tikzpicture}[scale=4]
    \inputtikz{somaCoordsPol}
  \end{tikzpicture}
}}

This translated into Racket becomes:

@lispcode[
(define (+pol p ro fi)
  (+xy p 
       (* ro (cos fi))
       (* ro (sin fi))))
]

Some examples of its use:

@incremental[
(+pol (xy 1 2) (sqrt 2) (/ pi 4))
(+pol (xy 1 2) 1 0)
(+pol (xy 1 2) 1 (/ pi 2))
]

@questions[
@question{The function @fn[=c] defined in @secref{exe:equalc} compares the
coordinates of two points, returning true if they are coincidental. However, 
taking into account that numeric operations can produce rounding errors it is
possible that two coordinates, which in theory should be the same, in practice
are not considered as such. For example, the point @${(-1,0)} in rectangular
coordinates can be expressed in polar coordinates as @${\rho=1, \phi=\pi} but
Racket will not consider them equal, which can be seen in the example below:

@incremental[
(=c (xy -1 0) (pol 1 pi))
(xy -1 0)
(pol 1 pi)
]

As you can see, although the coordinates are not the same they are very close,
i.e., the distance between them is very close to zero. Propose a new definition
for the function @fn[=c] based on the concept of distance between coordinates.
}]

@section{Bi-dimensional Geometric Modelling}

In this section we will be introducing some bi-dimensional geometric
modelling operations.

In order to visualize the shapes we create we need to have a CAD application,
like AutoCAD or Rhino. The choice of which CAD application we wish to use is
made using the @fn[backend] function together with the argument (@fn[autocad]
or @fn[rhino]). Therefore, a program that uses Rosetta usually starts with:

@verbatim{
#lang racket
(require (planet aml/rosetta))
(backend autocad)
}

or with:

@verbatim{
#lang racket
(require (planet aml/rosetta))
(backend rhino)
}

depending on the user's preference for AutoCAD or Rhino, respectively.

Let us start by considering the creation of three circles. For that we can use
the function @fn[circle] which receives the centre point and the radius as
arguments. In @figref{fig:circulos} you can see the result of the following program
in AutoCAD:@footnote{From here onwards, we will omit the header that requires
Rosetta and chooses the CAD application and, instead, we shall focus our attention
on the operations for geometric modelling.}

@verbatim{
#lang racket
(require (planet aml/rosetta))
(backend autocad)

(circle (pol 0 0) 4)
(circle (pol 4 (/ pi 4)) 2)
(circle (pol 6 (/ pi 4)) 1)
}

@figure[#:tag "fig:circulos"
        #:caption @elem{A series of circles.}]{
  @tex{
  \begin{tikzpicture}[scale=0.5]
    \draw (0:0cm)circle(4);
    \draw (45:4cm)circle(2);
    \draw (45:6cm)circle(1);
  \end{tikzpicture}
}}

Another much used operation is the one that creates line segments: @fn[line].
In its most simple form it takes the two positions of its extremities. However,
it is possible to invoke this function with any number of positions, case in
which it will successively connect each position with the next, forming a
polygonal line. @figref{fig:linhas} shows the example of a swastika
@footnote{The swastika is a mythical symbol, used by many cultures since
the neolithic period} produced by the following code:

@lispcode[
(line (xy -1 -1) (xy -1 0) (xy +1 0) (xy +1 +1))
(line (xy -1 +1) (xy 0 +1) (xy 0 -1) (xy +1 -1))
]

@figure[#:tag "fig:linhas"
        #:caption @elem{A set of line segments.}]{
  @tex{
  \begin{tikzpicture}
    \draw (-1,-1)--(-1,0)--(+1,0)--(+1,+1);
    \draw (-1,+1)--(0,+1)--(0,-1)--(+1,-1);
    % \draw (2,0)--(0,2);
    % \draw (1,2)--(3,2)--(2,3)--(2,1);
  \end{tikzpicture}
}}

In case we wish to draw @emph{closed} polygonal lines it is preferable
that we use the @fn[polygon] function, very similar to the function @fn[line]
but with the difference that it creates an additional segment connecting the
last position with the first. @Figref{fig:poligono} shows the result of the
following code:

@lispcode[
(polygon (pol 1 (* 2 pi 0/5))
         (pol 1 (* 2 pi 1/5))
         (pol 1 (* 2 pi 2/5))
         (pol 1 (* 2 pi 3/5))
         (pol 1 (* 2 pi 4/5)))
]

@figure[#:tag "fig:poligono"
        #:caption @elem{A polygon.}]{
  @tex{
  \begin{tikzpicture}
    \draw (0:1)--(72:1)--(144:1)--(216:1)--(288:1)--cycle;
  \end{tikzpicture}
}}

And for drawing @emph{regular} polygons, i.e., polygons that have equal edges
and angles, as shown in @figref{fig:poligono}, it is preferable to use the function
@fn[regular-polygon]. This function receives as arguments the number of sides,
its centre point, a radius, a rotation angle and a boolean to indicate if the radius
refers to an inscribed circle (i.e. the radius is the distance from the edges to the centre)
or a circumscribed circle (i.e. the radius is the distance from the vertexes to the
centre). If omitted, the centre point will be considered the origin, the radius will have
one unit of measurement, the angle will be considered zero and the circle will be circumscribed.

Using @fn[regular-polygon], @figref{fig:poligono} can be obtained by:

@lispcode[
(regular-polygon 5)
]

More interesting examples can be obtained by changing the rotation angle.
For example, the following expressions will produce the image shown in
@figref{fig:poligonosRegulares}:

@lispcode[
(regular-polygon 3 (xy 0 0) 1 0 #t)
(regular-polygon 3 (xy 0 0) 1 (/ pi 3) #t)
]
@lispcode[
(regular-polygon 4 (xy 3 0) 1 0 #t)
(regular-polygon 4 (xy 3 0) 1 (/ pi 4) #t)
]
@lispcode[
(regular-polygon 5 (xy 6 0) 1 0 #t)
(regular-polygon 5 (xy 6 0) 1 (/ pi 5) #t)
]

@figure[#:tag "fig:poligonosRegulares"
        #:caption @elem{Overlapping triangles, squares and pentagons with different rotation angles.}]{
  @tex{\autodrawing{poligonosB}}
}

For four sided polygons aligned with the @${X} and @${Y} axis there is a
very simple function: @fn[rectangle]. This function can either be used with
the position of its bottom left corner and upper right corner or the with the
position of its bottom left corner and the two rectangle dimensions, as exemplified
below and represented in @figref{fig:rectangulos}:

@lispcode[
(rectangle (xy 0 1) (xy 3 2))
(rectangle (xy 3 2) 1 2)
]

@figure[#:tag "fig:rectangulos"
        #:caption @elem{A set of rectangles}]{
  @tex{
  \begin{tikzpicture}
    \draw(0,1)rectangle(3,2);
    \draw(3,2)rectangle(4,4);
  \end{tikzpicture}
}}

In the following sections we will introduce the remaining functions
available in Rosetta.


@questions[
@question{Recreate the drawing presented in @figref{fig:circulos} but this time using rectangular coordinates.
}

@question{We wish to place two circles with unit radius and around
an origin so that the circles are tangent to each other as shown in
the following drawing:

@fig{
  @tex{
\begin{tikzpicture}[scale=0.5]
\draw[very thin](-2.5,0)--(2.5,0);
\draw[very thin](0,-2.5)--(0,2.5);
\draw (1,0) circle(1cm);
\draw (-1,0) circle(1cm);
\end{tikzpicture}
}}

Write a sequence of expressions that when evaluated produce the above image.}

@question{We wish to place four circles with unit radius and around
an origin so that the circles are tangent to each other as shown in
the following drawing:

@fig{
  @tex{
\begin{tikzpicture}[scale=0.5]
\draw[very thin](-2.5,0)--(2.5,0);
\draw[very thin](0,-2.5)--(0,2.5);
\draw (1,1) circle(1cm);
\draw (-1,1) circle(1cm);
\draw (1,-1) circle(1cm);
\draw (-1,-1) circle(1cm);
\end{tikzpicture}
}}

Write a sequence of expressions that when evaluated produce the above image.}

@question{We wish to place three circles with unit radius and around
an origin so that the circles are tangent to each other as shown in
the following drawing:

@fig{
  @tex{
\begin{tikzpicture}[scale=0.5]
\draw[very thin](-2.5,0)--(2.5,0);
\draw[very thin](0,-2.5)--(0,2.5);
\draw (0:1.1547cm) circle(1cm);
\draw (120:1.1547cm) circle(1cm);
\draw (240:1.1547cm) circle(1cm);
\end{tikzpicture}
}}

Write a sequence of expressions that when evaluated produce the above image.}
]

@section{Side Effects}

In Racket, as we have previously seen, every expression has a value. We can see
that by evaluating an arithmetic expression but also when evaluating a geometric
expression:

@incremental[(+ 1 2)(circle (xy 1 2) 3)]

@(clear-tikz)

In the last example, the result of evaluating the second expression is a geometric
entity. When Racket writes a result that is a geometric entity it uses a notation
based on the name of that entity and an integer to distinguish between them. Most
of the times we are not only interested in seeing a geometric entity as a piece of
text but we are also interested in @emph{visualizing} that entities in space. For
that purpose the evaluation of geometric expressions also has a @emph{side effect}:
all created geometrical forms are automatically added to the chosen CAD application
using the @fn[backend] function.

That way, evaluating the following program:

@verbatim{
#lang racket
(require (planet aml/rosetta))
(backend autocad)

(circle (xy 1 2) 3)
}

produces as a result an abstract value that represents a circle with a
radius of @${3} units, centred at the point @${1,2} and, as a side effect,
that circle becomes visible in AutoCAD.

This behaviour of geometric functions like @fn[circle], @fn[line],
@fn[rectangle], etc., is fundamentally different from the ones we have seen
so far because previously these functions were used to compute something, i.e.,
to produce a value, and now it is not that value that interests us the most
but rather its side effect (also called @emph{collateral effect}) that allows
us to visualize the geometrical shape in the CAD application.

One important aspect of using side effects is the possibility of their @emph{composition}.
The composition of side effects is accomplished through @emph{sequencing}, i.e., the
sequential computation of the different effects. In the next section we will
discuss sequencing of side effects.


@section{Sequencing}

So far, we have combined mathematical expressions using mathematical operators.
For example, from the expressions @lisp[(sin x)] and @lisp[(cos x)] we can calculate
their division by @lisp[(/ (sin x) (cos x))]. This is possible because the evaluation
of the sub-expressions @lisp[(sin x)] and @lisp[(cos x)] will produce two values
that can then be used in the division.

With geometrical functions these kinds of combinations must be done differently since, as
we saw, their evaluation also produces side effects. Seeing as these side effects are 
precisely what we want, Racket provides a way of producing them sequentially, i.e., one
after the other. That feature is called @stx[begin] and is used for @emph{sequencing} side
effects. For example, consider a function that draws a circle with a radius @${r}, centred
on @${P} with an inscribed or circumscribed square, depending on the user's specification, as
we show in @figref{fig:circQuad}. 

@figure[#:tag "fig:circQuad"
        #:caption @elem{A circle with a square inscribed (left) and a square circumscribed (right).}]{
  @tex{
  \begin{tikzpicture}[scale=2]
    \inputtikz{circQuad}
  \end{tikzpicture}
}}

The function's definition could start as something like:

@lispcode[
(define (circle-square p r inscribed?)
  ...)
]

The drawing produced by the function will naturally depend on the logic value of @lisp[inscribed?],
that is, this function's definition could be something like:

@lispcode[
(define (circle-square p r inscribed?)
  (if inscribed?
    "create a circle and a square inscribed in that circle"
    "create a circle and a square circumscribed in that circle"))
]

The problem now is that, for each case of @stx[if], the function must generate
@emph{two} side effects, namely create a circle and create a square. But the @stx[if]
only admits one expression, making us wonder how we would be able to combine two
side effects in a single expression. For this purpose Racket provides the form S@stx[begin].
This operator will take any number of expressions and will sequentially evaluate them, i.e.,
one after the other, returning the value of the last. Logically, if only the value of the
last expression is used, then all other values from the other expressions are discarded
and these are only relevant for the side effect they may have produced.

Using the @stx[begin] operator we can further detail our function:

@lispcode[
(define (circle-square p r inscribed?)
  (if inscribed?
    (begin
      "create a circle"
      "create a square inscribed in the circle")
    (begin
      "create a circle"
      "create a square circumscribed in the circle")))
]

All we need to do now is translate each expressions into the corresponding
functions. In the case of the inscribed square we will use polar coordinates
because we know its vertices will be set in a circle, one with @${\frac{\pi}{4}}
and another at @${\pi+\frac{\pi}{4}}.

@lispcode[
(define (circle-square p r inscribed?)
  (if inscribed?
    (begin
      (circle p r)
      (rectangle (+pol p r (* 5/4 pi)) (+pol p r (* 1/4 pi))))
    (begin
      (circle p r)
      (rectangle (+xy p (- r) (- r)) (+xy p r r)))))
]

Note that for the @stx[if] operator, both the consequence and the alternative
are one single expression even though each of those expressions is the result
of two other more elemental expressions. The @stx[begin] operator can therefore be
seen as a mechanism for grouping expressions.

Even though sequencing expressions requires the use of the operator @stx[begin],
it is usually possible to minimize its use in these expressions. An attentive look
at the previous function @stx[circle-square] shows that a circle is always created
independently of the square being inscribed or circumscribed. That way we can redefine
the function so that the circle is created outside the @stx[if]:

@lispcode[
(define (circle-square p r inscribed?)
  (begin
    (circle p r)
    (if inscribed?
      (begin
        (rectangle (+pol p r (* 5/4 pi)) (+pol p r (* 1/4 pi))))
      (begin
        (rectangle (+xy p (- r) (- r)) (+xy p r r))))))
]

It is now clear that the two @stx[begin]s of the @stx[if] form only
have one expression each so its use is unnecessary. We can therefore
simplify the expression to:

@lispcode[
(define (circle-square p r inscribed?)
  (begin
    (circle p r)
    (if inscribed?
      (rectangle (+pol p r (* 5/4 pi)) (+pol p r (* 1/4 pi)))
      (rectangle (+xy p (- r) (- r)) (+xy p r r)))))
]
 
Even tough the operator @stx[begin] is necessary every time we wish to
evaluate more than one expression, there are some situations where
Racket uses an @emph{implicit} @stx[begin], such as in a function's
body. In fact, when a function is invoked every expression in the
function's body is evaluated sequentially and the function's value is
only determined by the last expression. This fact allows us to
simplify the @lisp[circle-square] function even more:

@lispcode[
(define (circle-square p r inscribed?)
  (circle p r)
  (if inscribed?
    (rectangle (+pol p r (* 5/4 pi)) (+pol p r (* 1/4 pi)))
    (rectangle (+xy p (- r) (- r)) (+xy p r r))))
]

@questions[

@question{Define a function called @fn[circle-and-radius] that given
the coordinates of its centre point and the radius creates the specified
circle and, as shown in @figref{fig:circulos}, places the text describing
the radius on the circle's right side. The text should be proportional to
the circle's size.}

@question{Using the previously defined function
@lisp[circle-and-radius], recreate @figref{fig:circulos}.}
]

@section[#:tag "sec:ordemDorica"]{Doric Order}

@figure[#:tag "fig:segesta"
        #:caption @elem{The Doric Order exemplified in the Greek Temple of Segesta. This temple was never ended 
   Photography by Enzo De Martino.}]{
  @authorizedPhoto{segesta/EnzoDeMartino}
}

In @Figref{fig:segesta} we see an image of the Segesta Greek temple.
This temple, which was never finished, was built during the 5th century
D.C, and represents a fine example of the Doric order, the oldest of the
three orders of Greek architecture. In this order a column is composed of
a shaft, an Echinus and an abacus. The abacus is shaped like a squared plaque
that stands on top the Echinus, the Echinus is similar to an inverted cone
frustum which stands on top the shaft, and the shaft is similar to a cone
frustum with twenty @emph{flutes} around it. These flutes are semi-circular
shaped and carved along the shaft. @footnote{These columns also present an
intentional deformation called @emph{entasis}. The entasis is a small curvature
give to columns and is believed to be a way a creating a optical illusion in
order to misguide the eye to see the column curved instead of straight.} When
the Romans adopted the Doric order they introduced some modifications, in
particular to the flutes which, in many cases, were simply removed.

Even though we are particularly interested in three.dimensional modelling, for
pedagogic reasons, we are going to start by sketching a Doric column
(without flutes) in two dimensions. In the following sections we will
extend this process to create a three-dimensional model.

The same way a Doric columns can be decomposed into its basic components -
shaft, Echinus and abacus - the drawing of one can too be decomposed in
components. Therefore, we will create functions to draw the shaft, Echinus
and abacus. @figref{fig:modeloColuna} shows a reference model. Let us start
by defining a function for the shaft.

@figure[#:tag "fig:modeloColuna"
        #:caption @elem{The Doric column for reference}]{
  @tex{
  \begin{tikzpicture}[scale=0.6]\small
    \inputtikz{modeloColuna}
  \end{tikzpicture}
}}

@lispcode[
(define (shaft)
  (line (xy -0.8 10)
        (xy -1 0) 
        (xy 1 0)
        (xy 0.8 10)
        (xy -0.8 10)))
]

In this example, we used the @fn[line] function that, given a sequence of positions,
will create a line with vertices on those positions. Another possibility, probably more
appropriate, would be to create a @emph{closed} polygonal line, something we can do with
the @fn[polygon] function, omitting the repeated position, i.e.:

@lispcode[
(define (shaft)
  (polygon (xy -0.8 10)
           (xy -1 0) 
           (xy 1 0)
           (xy 0.8 10)))
]

To complete the figure we also need a function for the Echinus and another for the abacus.
The reasoning for the Echinus is similar:

@lispcode[
(define (echinus)
  (polygon (xy -0.8 10)
           (xy -1 10.5) 
           (xy 1 10.5)
           (xy 0.8 10)))
]

For the abacus we could use a similar strategy or explore a function in Rosetta that
creates rectangles. This function requires two points to define a rectangle:

@lispcode[
(define (abacus)
  (rectangle (xy -1 10.5)
             (xy 1 11)))
]

Finally, the expression that creates a Doric column:

@lispcode[
(define (doric-column)
  (shaft)
  (echinus)
  (abacus))
]

Note that the @fn[doric-column] function sequentially calls the functions @fn[shaft],
@fn[Echinus] and finally @lisp[abacus].

@Figref{fig:coluna} shows the result of invoking the @fn[doric-column] function.

@figure[#:tag "fig:coluna"
        #:caption @elem{A Doric column.}]{
   @tex{\autodrawing[0.1]{coluna}}
}

@section{Parametrization of Geometric Figures}

Unfortunately, the Doric column we created has fixed position and size, making
it difficult to use this function in different contexts. Naturally, this function
would be much more useful if its creating was @emph{parametrized}, i.e., if its
creation depended on the parameters that describe a column, as for example, the
column's  base coordinates, the height of the Echinus, shaft and abacus, the base
and top Echinus radius, etc.

In order to better understand this parametrization of these functions, let us start by
considering the shaft represented in @figref{fig:coordenadasFuste}.

@figure[#:tag "fig:coordenadasFuste"
        #:caption @elem{Sketch of a column's shaft.}]{
  @tex{
  \begin{tikzpicture}[scale=0.8]
    \inputtikz{coordenadasFuste}
  \end{tikzpicture}
}}

The first step in parametrizing a geometrical drawing is to correctly
identify the relevant parameters. In the case of the shaft one of the
obvious parameters would be its spatial location, i.e., the coordinates
of a reference point in relation to which the shaft will be drawn. Let us
then consider that the shaft will be placed with its base's centre point at
an imaginary point @${P} of arbitrary coordinates @${(x,y)}. In addition to
this parameter we also need know the height of the shaft @${a} and both
base @${r_b} and top radius @${r_t}.

To make the drawing process easier it is convenient to consider additional
reference points in our sketch. For the shaft, since it's shaped essentially
like a trapeze, we can look at its shape as the succession of line segments along
a sequence of points @${P_1}, @${P_2}, @${P_3} and @${P_4}, points which we
can easily calculate from @${P}.

We now have all we need to define a function that draws the column's
shaft. To make the program clearer we will use the names @lisp[a-shaft] for the
height @${a}, the base radius @lisp[r-base] and top radius @lisp[r-top]
respectively. The definition will now be:

@lispcode[
(define (shaft p a-shaft r-base r-top)
  (polygon (+xy p (- r-top) a-shaft)
           (+xy p (- r-base) 0)
           (+xy p (+ r-base) 0)
           (+xy p (+ r-top) a-shaft)))
]

Next we need to draw out the Echinus. It is convenient to consider once more
a geometrical sketch, as shown in @figref{fig:coordenadasCoxim}.

@figure[#:tag "fig:coordenadasCoxim"
        #:caption @elem{Sketch of an Echinus.}]{
  @tex{
  \begin{tikzpicture}[scale=0.8]
    \inputtikz{coordenadasCoxim}
  \end{tikzpicture}
}}

As similar to the shaft, from the bases' centre point @${P} we can compute the
coordinates that define the extremities of the lines that draw an Echinus. Using
those points, the function will be:

@lispcode[
(define (echinus p a-echinus r-base r-top)
  (polygon (+xy p (- r-base) 0)
           (+xy p (- r-top) a-echinus)
           (+xy p (+ r-top) a-echinus)
           (+xy p (+ r-base) 0)))
]
 
Having done the shaft and Echinus all that is left now is to draw the Abacus. For
that we can consider yet another geometric sketch, shown in @figref{fig:coordenadasAbaco}.

@figure[#:tag "fig:coordenadasAbaco"
        #:caption @elem{Sketch of an Abacus.}]{
  @tex{
  \begin{tikzpicture}[scale=0.8]
    \inputtikz{coordenadasAbaco}
  \end{tikzpicture}
}}

Once more we will consider @${P} as the starting point in the Abacus's base. From this
point we can easily calculate the points @${P_1} and @${P_2}, that are the two opposite
corners of the rectangle that defines the Abacus. That way, we have:

@lispcode[
(define (abacus p a-abacus l-abacus)
  (rectangle (+xy p (/ l-abacus -2) 0)
             (+xy p (/ l-abacus +2) a-abacus)))
]

Finally, to create the entire column we must combine the functions that draw the shaft,
the Echinus and the Abacus. We need only to take into account that, as
@figref{fig:coordenadasColuna} shows, the shaft's top radius is coincidental with
the Echinus' base radius and the Echinus top radius is half the width of the
Abacus. The figure also shows that the coordinates of the Abacus' base is the result of
adding the shaft's height to the the coordinates of the the shaft's base and that the Abacus's
base coordinates are the result of adding the combined heights of the shaft and Echinus to
the shaft's base coordinates.

@figure[#:tag "fig:coordenadasColuna"
        #:caption @elem{Composition of the shaft, Echinus and Abacus.}]{
  @tex{
  \begin{tikzpicture}[scale=0.8]\smaller[2]
    \inputtikz{coordenadasColuna}
  \end{tikzpicture}
}}

As we did before, let us give more appropriate names to the parameters
in @figref{fig:coordenadasColuna}.  Using the names @lisp[p],
@lisp[a-shaft], @lisp[r-base-shaft], @lisp[a-Echinus],
@lisp[r-base-Echinus], @lisp[a-abacus] and @lisp[l-abacus] instead of
the corresponding, @${P}, @${a_f}, @${r_{bf}}, @${a_c}, @${r_{bc}},
@${a_a} and @${l_a}, we obtain:

@lispcode[
(define (column p
                a-shaft r-base-shaft
                a-echinus r-base-echinus
                a-abacus l-abacus)
  (shaft p a-shaft r-base-shaft r-base-echinus)
  (Echinus (+xy p 0 a-shaft) a-echinus r-base-echinus (/ l-abacus 2))
  (abacus (+xy p 0 (+ a-shaft a-echinus)) a-abacus l-abacus))
]

Using this function we can easily explore different variations of columns. The following examples
reproduce what is shown in @figref{fig:colunasDoricas}.

@lispcode[
(column (xy  0 0) 9 0.5 0.4 0.3 0.3 1.0)
(column (xy  3 0) 7 0.5 0.4 0.6 0.6 1.6)
(column (xy  6 0) 9 0.7 0.5 0.3 0.2 1.2)
(column (xy  9 0) 8 0.4 0.3 0.2 0.3 1.0)
(column (xy 12 0) 5 0.5 0.4 0.3 0.1 1.0)
(column (xy 15 0) 6 0.8 0.3 0.2 0.4 1.4)
]

@figure[#:tag "fig:colunasDoricas"
        #:caption @elem{Multiple Doric columns.}]{
   @tex{\autodrawing{colunas}}
}

As we can see, not all columns obey the proportion's canon of the Doric order.
Further ahead we are going to see which modifications are needed to avoid this problem.

@section{Documentation}

In the @lisp[column] function, @lisp[a-shaft] is the shaft's height,
@lisp[r-base-shaft] is the shaft's base radius, @lisp[r-top-shaft] is
the shaft's top radius, @lisp[a-echinus] is the Echinus' height and,
finally, @lisp[r-abacus] is the Abacus' radius. Because the function
already employs many parameters and its meaning may not be clear to someone
that reads the function's definition for the first time, it is convenient
to @emph{document} the function. For that, Racket provides a special syntax:
each time a semicolon @tt{;} is used Racket will ignore everything that is 
in front of it until the end of that line. That allows text to be written
between the code without Racket trying to interpret its meaning.

Using documentation, the full program for creating Doric columns will look
something like what is shown below. Note that this example tries to show
the different ways documentation can be used in Racket and not a specific
example of a documented program.@footnote{The program is so simple in fact that
it should not require so much documentation.}

@verbatim{
;;;;Drawing Doric Columns

;;;The drawing of a Doric column is divided into three parts:
;;;shaft, Echinus and abacus. Each of those parts has an   
;;;independent function.

;Draws the shaft of a Doric column.
;p: column's centre base coordinate,
;a-shaft: shaft's height,
;r-base: shaft's base radius,
;r-top: shaft's top radius.
(define (shaft p a-shaft r-base r-top)
  (polygon (+xy p (- r-top) a-shaft)
           (+xy p (- r-base) 0)
           (+xy p (+ r-base) 0)
           (+xy p (+ r-top) a-shaft)))

;Draws the Echinus of a Doric column.
;p: Echinus centre base coordinate,
;a-echinus: Echinus height,
;r-base: Echinus base radius,
;r-top: Echinus top radius.
(define (Echinus p a-Echinus r-base r-top)
  (polygon (+xy p (- r-base) 0)
           (+xy p (- r-top) a-echinus)
           (+xy p (+ r-top) a-echinus)
           (+xy p (+ r-base) 0)))

;Draws the Abacus of a Doric column.
;p: column's centre base coordinate,
;a-abacus: abacus height,
;l-abacus: abacus width.
(define (abacus p a-abacus l-abacus)
  (rectangle (+xy p (/ l-abacus -2) 0)
             (+xy p (/ l-abacus +2) a-abacus)))

;Draws a Doric column composed of a shaft, an Echinus and an Abacus.
;p: column's centre base coordinate,
;a-shaft: shaft's height,
;r-base-shaft: shaft's base radius,
;r-base-echinus: Echinus base radius = shaft's top radius,
;a-echinus: Echinus height,
;a-abacus: abacus height,
;l-abacus: abacus width = 2*Echinus top radius.
(define (column p
                a-shaft r-base-shaft
                a-echinus r-base-echinus
                a-abacus l-abacus)
  ;;We draw the shaft at the base point p
  (shaft p a-shaft r-base-shaft r-base-echinus)
  ;;We place the Echinus on top of the shaft
  (Echinus (+xy p 0 a-shaft) a-echinus r-base-echinus (/ l-abacus 2))
  ;;and the Abacus on top of the Echinus
  (abacus (+xy p 0 (+ a-shaft a-echinus)) a-abacus l-abacus))
}

When a program is documented, is easier to understand what the program
~does, without having to study the functions' body. As we can see in the
following example, it is common pragmatic in Racket to use a different number
of semicolons to indicate the importance of the comment:

@itemlist[
@item{@verbatim{;;;;} They should be placed on the left margin and are used to divide the program into sections and to give a title to each one.}
@item{@verbatim{;;;} They also should be placed on the left margin and are used for general commenting of the program that will follow. These should not be used inside functions.}
@item{@verbatim{;;} Should be aligned with the correspondent piece of code, which appears immediately below.}
@item{@verbatim{;} Should be placed in a same column to the right, referencing the piece of code immediately to the left.}
]

It is important that we get used to documenting our programs but it is important
to note that excessive documentation can also have disadvantages:

@itemlist[
@item{The code in Racket should be clear enough for a human being to understand. It is better to spend more time writing clearer code than to write documentation that explains it.}
@item{Having documentation that does not correspond to the program is worse than having no documentation at all.}
@item{Frequently, we need to modify our programs in order to adapt them to different purposes. The more documentation we have, the more documentation we will have to modify to make it relevant with the changes made to the program.}]

For these reasons, we should try to write the code as clear as possible and
at the same time provide short and useful documentation: the documentation should
not say what is already obvious from reading the program.

@questions[
@question{Consider an arrow with an origin in @${P}, a length of @${\rho},
an inclination @${\alpha} , an opening angle @${\beta} and a tip  @${\sigma},
as shown in the following figure:

@fig{
  @tex{
\begin{tikzpicture}[scale=1.5]
\inputtikz{seta}
\end{tikzpicture}
}}

Define a function @lisp[arrow] that, given the parameters @${P},
@${\rho}, @${\alpha}, @${\beta} and @${\sigma} creates the
corresponding arrow.}

@question{
Based on the previous exercise define a function that given the point
@${P}, the length @${\rho} and the angle @${\alpha} draws "the North"
as shown in the following figure:

@fig{
  @tex{
\begin{tikzpicture}[scale=0.5]
\draw[thick] (-0.4142,-0.4142)--(2,2)+(0.5,0.5)node[rotate=-45]{\Huge N};
\draw[thick] (2,2)--(2,0)--(0,2)--cycle; 
\end{tikzpicture}
}}

This drawing should also obey the following constraints:
@itemlist[
@item{The opening angle @${\beta} must be @${45\,^{\circ}.}}
@item{The arrow's tip length @${\sigma} must be @${\frac{\rho}{2}}.}
@item{The "N" should be distanced @${\frac{\rho}{10}} from the arrow's tip in the arrow's direction}
@item{The size of "N" should be half the length @${\rho}.}]
}

@question{
Using the function @fn[arrow] function, define a new function called
@lisp[arrow-from-to] that given two points, creates an arrow that goes from
the first point to the second. coordinate. The tips should have one unit as measurement
and the angle should be @${\frac{\pi}{8}}.}

@question{Consider a house with only rectangular rooms. Define the function
@lisp[rectangular-divisions] that receives as  parameters the bottom left corner
of a room, the length and width of the room and a text describing that room's type
of use. With those values, that function should build a rectangle with two text lines, one with
the rooms type of use and the other with the room's floor area. For example, the following code lines:

@def/no-show[
(define (rectangular-division p comprimento largura funcao)
  (rectangle p (+xy p comprimento largura))
  (text-centered funcao
                 (+xy p (* comprimento 0.5) (* largura 0.7))
                 (/ comprimento 8))
  (text-centered (format "Area:~A" (real->decimal-string (* comprimento largura) 2))
                 (+xy p (* comprimento 0.5) (* largura 0.3))
                 (/ comprimento 10)))
]

@def/no-results[
(rectangular-division (xy 0 0) 4 3 "kitchen")
(rectangular-division (xy 4 0) 2 3 "pantry")
(rectangular-division (xy 6 0) 5 3 "room")
(rectangular-division (xy 0 5) 5 4 "living-room")
(rectangular-division (xy 5 5) 3 4 "toilet")
(rectangular-division (xy 8 3) 3 6 "room")
]
 
They produce as a result the following drawing:

@(def/tikz)}]

@section[#:tag "sec:Depur"]{Debugging}

As we all know @emph{Errare humanum est}. Making mistakes is part of our day-to-day
life and we mostly learn how to deal with them. The same does not apply to programming
languages. Any mistake made in a computer program will result in it having a different
behaviour than that which was to be expected.

Seeing how easy it is to make errors, it should also be easy to detect
and correct them.  The process of detecting and correcting errors is
called @emph{debugging}. Different programming languages provide
different mechanisms to do that. As we will see Racket is well covered
for these mechanisms.

Generally, errors in programs can be classified as @emph{syntactic errors} or @emph{semantic errors}.

@subsection{Syntactic Errors}

Syntactic errors occur each time we write a line of code which does not follow the
language's grammar rules. As an example let us suppose we wish to create a function to
create a single Doric column, which we will refer as @emph{standard}, and that always has
the same dimensions, dispensing the need for any other parameters. One possible
definition is:

@lispcode[
(define (standard-column
  (column (xy 0 0) 9 0.5 0.4 0.3 0.3 0.5)))
]

However, if we test this definition, Racket will produce an error, telling us 
that something is wrong:

@lispcode[
define: not an identifier, identifier with default, or keyword for 
procedure argument in: (column (xy 0 0) 9 0.5 0.4 0.3 0.3 0.5)
]

The error Racket is warning us about is that the form @lisp[define] does not
follow the required syntax, and in fact a careful look at it shows we have not
followed the correct syntax that, as we saw in section @ref{sec:syntaxDefine}, should
obey the following structure:

@lispcode[
(define (_name #,(lispemphi parameter "1") ... #,(lispemphi parameter "n"))
  _body)
]

Our mistake is now obvious, we forgot to close the parentheses in the list of
parameters. That makes Racket use the following expression as part of the
parameters list and, being unable to do so, Racket reports a syntactic error, i.e.,
a "sentence" which does not obey the language syntax.

There are several other types of errors that Racket is capable of detecting and
these will be discussed as we go along. The important thing is not that what kind
of syntactic errors Racket is capable of detecting but rather that Racket is capable
of checking the expressions we write and detect syntactic errors in them before
evaluating them.

@subsection{Semantic Errors}

Semantic errors are very different from syntactic ones. A semantic error is not the
misspelling of a "sentence" but rather a mistake in its meaning. In other terms, a semantic
error occurs when we write a sentence which we think has a certain meaning but in reality
it has a different one.

Generally semantic errors can only be detected during the evaluation of the functions that
contain them. Part of the semantic errors are detectable by Racket's evaluator but countless
others can only be detected by the programmer.

As an example of a semantic error consider a meaningless operation such as the sum of a number
with a @emph{string}:

@incremental[(+ 1 "two")]

As we can see the mistake is explained in the given message, which says that the second argument
should be a number. In this example the mistake is obvious enough for us to detect it immediately.
However, with more complex programs the process of finding mistakes can often be slow and frustrating.
It is also a fact that the more experienced we get at detecting errors the faster we will be at doing it..

@section{Three-dimensional Modelling}

As you have seen in the previous section, Rosetta provides multiple drawing tools (lines, rectangles, circles, etc.)
that allows us to easily draw bi-dimensional representations of objects such as floor sections, side sections, etc.

Although to this moment we have only used Rosetta's bi-dimensional drawing capacities it is possible to
go further and start exploring @emph{three-dimensional modelling}. This type of modelling represents lines,
surfaces and volumes in the three-dimensional space.

In this section we will study Rosetta's functions the allow the creating of such three-dimensional objects.

@subsection{Predefined Solids}

Rosetta provides a set of predefined functions that create solids from their three-dimensional coordinates.
Although these predefined functions only allow the creation of a limited number of solids they are enough
to create sophisticated models.

The predefined operations can create boxes (function @fn[box]), cylinders (function @fn[cylinder]),
cones (function @fn[cone]), spheres (function @fn[sphere]), toruses (function @fn[torus]) and 
pyramids (function @fn[regular-pyramid]). Each of these functions accepts various optional arguments for
creating solids in these solids in different ways. @\figref{fig:solidosPrimitivos} shows a set of
solids built from the following expressions:

@lispcode[
(box (xyz 2 1 1) (xyz 3 4 5))
(cone (xyz 6 0 0) 1 (xyz 8 1 5))
(cone-frustum (xyz 11 1 0) 2 (xyz 10 0 5) 1)
(sphere (xyz 8 4 5) 2)
(cylinder (xyz 8 7 0) 1 (xyz 6 8 7))
(regular-pyramid 5 (xyz -2 1 0) 1 0 (xyz 2 7 7))
(torus (xyz 14 6 5) 2 1)
]

@figure[#:tag "fig:solidosPrimitivos"
        #:caption @elem{Primitive solids in Rosetta.}]{
  @autoimage{solidos}
}

@figure[#:tag "fig:piramideGize"
        #:caption @elem{Great Pyramid of Giza. Photography by Nina Aldin Thune.}]{
  @authorizedPhoto{piramides/Kheops-Pyramid}
}

It is interesting to notice that some of the most important works in architectural history could be modelled
directly  using these operations. One example is the Great Pyramid of Giza, @\figref{fig:piramideGize}, built
over forty five centuries ago and an almost perfect example of a square pyramid. In its original form, the
Great Pyramid had a square base, with sides measuring 230 meters, and a height of 147 meters which made
it the tallest man-made structure up until the construction of the Eiffel Tower. Many other Egyptian pyramids
have similar proportions which makes them easy to model with the following function:

@lispcode[
(define (egyptian-pyramid p side height)
  (regular-pyramid 4 p (/ side 2) 0 height #f))
]

The case of the Great Pyramid of Giza would be:

@lispcode[
(egyptian-pyramid (xyz 0 0 0) 230 147)
]

Contrary to the Great Pyramid of Giza, there are not many examples that can be modelled with a single geometric
primitive. It is however possible to create more complex structures by combining these primitives.

Take for example the cross in @figref{fig:troncosCone}, built using six identical truncated cones, with their base
positioned at the same point and the top vertices positioned on orthogonal axis.

@figure[#:tag "fig:troncosCone"
        #:caption @elem{Cross, overlapping truncated cones.}]{
  @autoimage[#:scale 0.6]{cruzeta1}
}

To model this solid we will parametrize the base point @${p} and the other cone's dimensions : the base radius @${r_b}, top
radius @${r_t} and length @${c}:

@lispcode[
(define (cross p rb rt c)
  (cone-frustum p rb (+x p c) rt)
  (cone-frustum p rb (+y p c) rt)
  (cone-frustum p rb (+z p c) rt)
  (cone-frustum p rb (+x p (- c)) rt)
  (cone-frustum p rb (+y p (- c)) rt)
  (cone-frustum p rb (+z p (- c)) rt))
]

@questions[
@question{
Using the function @fn[cross] determine approximate values for the parameters as to create the following models:

@fig{@autoimage[#:scale 0.4]{cruzeta0} @autoimage[#:scale 0.4]{cruzeta2}}
}

@question{Using the function @fn[cone-frustum] define the function @fn[hourglass] function that given the base
centre point, the base radius, the neck radius and the height, creates the model of a hourglass similar to the
following example:

@fig{@autoimage[#:scale 0.3]{ampulheta}}
}]

Similar to the truncated cone, but with a polygonal base, we have the truncated pyramid. A truncated pyramid can
be created using the function @fn[regular-pyramid-frustum]. For that we must specify the number of sides, the base
centre point, the radius of the circle that circumscribes the base, the base rotation angle, the top centre point and
the radius of the circle that circumscribes the top.

@figref{fig:troncosPiramide} you can see the rotation's angle effect
given by the following expressions:

@figure[#:tag "fig:troncosPiramide"
        #:caption @elem{Three truncated pyramids with different rotation angles. From left to right the angles are: @${0}, @${\frac{\pi}{4}} e @${\frac{\pi}{2}}.}]{
  @autoimage{troncosPiramide}
}

@lispcode[
(regular-pyramid-frustum 3 (xyz 0 0 0) 2 0 (xyz 0 0 1) 1)
(regular-pyramid-frustum 3 (xyz 8 0 0) 2 pi/4 (xyz 8 0 1) 1)
(regular-pyramid-frustum 3 (xyz 16 0 0) 2 pi/2 (xyz 16 0 1) 1)
]

@figure[#:tag "fig:piramideRomboidal"
        #:caption @elem{The Dahshur pyramid. Photography by Ivrienen.}]{
  @authorizedPhoto{piramides/Snefru_Bent_Pyramid_in_Dahshur}
}

For a more architectural example let us consider the Dahshur @emph{rhomboidal} pyramid, illustrated in
@figref{fig:piramideRomboidal}, characterized for having sloped edges that abruptly change from @${43^{\circ}}
to @${55^{\circ}}, presumably to avoid its collapse due to the original slope. This pyramid is @${188.6}
meters wide and @${101.1} meters tall and it can be decomposed into two geometrical forms, an initial truncated
pyramid onto of which stands a quadrangular pyramid.

In order to generalise the modeling of this type of pyramid we can consider the schematic model in
@Figref{fig:esquemaPiramide} where a section of this pyramid is shown. From there it is easy to see
that @${h_0 + h_1 = h} and that @${l_0+l_1=l}. Moreover we have the trigonometric functions

@$${\tan\alpha_0=\frac{h_0}{l_0}\qquad\qquad \tan\alpha_1=\frac{h_1}{l_1}}

As a result, we have

@$${l_0=\frac{h-l\tan\alpha_1}{\tan\alpha_0-\tan\alpha_1}}

@figure[#:tag "fig:esquemaPiramide"
        #:caption @elem{Section of a rhomboidal pyramid.}]{
  @tex{
  \begin{tikzpicture}[scale=3]
    \inputtikz{esquemaPiramide}
  \end{tikzpicture}
}}

Transcribing these functions into Racket we have:

@lispcode[
(define (rhomboidal-pyramid p l h a0 a1)
  (define l0 (/ (- h (* l (tan a1))) (- (tan a0) (tan a1))))
  (define l1 (- l l0))
  (define h0 (* l0 (tan a0)))
  (define h1 (- h h0))
  (regular-pyramid-frustum 4 p l 0 h0 l1)
  (regular-pyramid 4 (+z p h0) l1 0 h1))
]

The rightmost model in @Figref{fig:piramidesRomboides} shows a Dahshur pyramid created by the following expression:

@lispcode[
(rhomboidal-pyramid (xyz 0 0 0) (/ 186.6 2) 101.1
                    (radians<-degrees 55) (radians<-degrees 43))
]

In the same image, to the left, two other pyramids were created with the following expressions:

@lispcode[
(rhomboidal-pyramid (xyz 300 0 0) (/ 186.6 2) 101.1 
                    (radians<-degrees 75) (radians<-degrees 40))
(rhomboidal-pyramid (xyz 600 0 0) (/ 186.6 2) 101.1
                    (radians<-degrees 95) (radians<-degrees 37))
]

@figure[#:tag "fig:piramidesRomboides"
        #:caption @elem{Three different rhomboidal pyramids.}]{
  @autoimage{piramidesRomboides}
}

@figure[#:tag "fig:washingtonMonument"
        #:caption @elem{Washington Monument. Photography by David Iliff.}]{
  @authorizedPhoto[#:scale 0.8]{Washington_Monument_Dusk_Jan_2006}
}

@questions[ 
@question{An obelisk is a monument shaped like a
rhomboidal pyramid. The Washington Monument, @figref{fig:washingtonMonument}, is a modern example of an obelisk of
enormous size, which can be defined, (relative to @figref{fig:esquemaPiramide}) with a truncated pyramid @${2l=16.8}
meters wide on the bottom and @${2l_1=10.5} meters wide on top, a total height of @${169.3} meters and an upper pyramid
top height of @${h_1=16.9} meters.

Define the @fn[obelisk] function that given the base centre point, base width, base height, top width and total height,
creates an obelisk.}

@question{A perfect obelisk follows a set of proportions in which its height is equal to the pyramid's base width,
which in turn is one-tenth of the total height. The top width has to be two thirds of the base width. With these
proportions in mind, define the @fn[perfect-obelisk] function, that given a base centre point and the total height,
creates a perfect obelisk.}

@question{Using the @fn[regular-pyramid-frustum] function, define a function called @lisp[prism] that creates a regular
prismatic solid. The function should receive as parameters the number of sides, the three-dimensional coordinates of the
base's centre point, the distance between that centre point to each vertices, the base rotation angle and the three-dimensional
coordinates of the top's centre point. As examples consider the following expressions:

@lispcode[
(prism 3 (xyz  0  0 0) 0.4 0 (xyz  0  0 5))
(prism 5 (xyz -2  0 0) 0.4 0 (xyz -1  1 5))
(prism 4 (xyz  0  2 0) 0.4 0 (xyz  1  1 5))
(prism 6 (xyz  2  0 0) 0.4 0 (xyz  1 -1 5))
(prism 7 (xyz  0 -2 0) 0.4 0 (xyz -1 -1 5))
]
which produce the following image:
@fig{@autoimage[#:scale 0.5]{prismas}}
}

@question{
The @emph{Sears Tower} shown in @figref{fig:SearsTower} (today called @emph{Willis Tower} was for many years the
tallest building in the world. This tower consists of nine square prisms, with different heights @${h_{i,j}} connected
to each other.

@figure[#:tag "fig:SearsTower"
        #:caption @elem{The @emph{Sears Tower}, em Chicago.}]{
  @authorizedPhoto[#:scale 0.6]{SearsTower}
}

From a top view these nine blocks define a square with a side @${l}, as shown in the following sketch:
  
@fig{
  @tex{
\begin{tikzpicture}
\inputtikz{searsTower}
\end{tikzpicture}
}}

Using the function @fn[prism] defined in the previous exercise, define a function called @lisp[sears-tower] capable
of creating buildings similar to the @emph{Sears Tower}. The function should have as parameters the three-dimensional
coordinates of the base corner @${P}, the base width @${l} and nine more parameters relative to each height value
@${h_{0,0}, h_{0,1},\ldots,h_{2,2}}.

The real @emph{Sears Tower} has the following parameters: @${l=68.7
 m}, @${h_{0,1}=h_{1,1}=442 m}, @${h_{1,0}=h_{2,1}=h_{1,2}=368 m},
 @${h_{0,0}=h_{2,2}=270 m} and @${h_{0,2}=h_{2,0}=205 m} as shown in the following image:

@fig{@autoimage[#:scale 0.12]{searsTowerC}}}
]

Besides the already presented geometrical primitives there is another that allows the creation of @emph{cuboid}
solids, i.e., solids with six faces but with a shape that is not necessarily cubic. A cuboid is defined by its
eight vertices divided into two sets of four, respectively the base and top vertices (in counter-clockwise order.
The following expressions produce the three cuboids presented in @figref{fig:cuboides}:

@figure[#:tag "fig:cuboides"
        #:caption @elem{Three cuboid solids with different vertexes}]{
  @autoimage{cuboidsA}
}

@lispcode[
(cuboid (xyz 0 0 0) (xyz 2 0 0) (xyz 2 2 0) (xyz 0 2 0)
        (xyz 0 0 2) (xyz 2 0 2) (xyz 2 2 2) (xyz 0 2 2))

(cuboid (xyz 4 0 0) (xyz 5 0 0) (xyz 5 2 0) (xyz 4 2 0)
        (xyz 3 1 2) (xyz 5 1 2) (xyz 5 2 2) (xyz 3 2 2))

(cuboid (xyz 7 2 0) (xyz 8 0 0) (xyz 8 3 0) (xyz 6 3 0)
        (xyz 7 2 2) (xyz 8 0 2) (xyz 8 3 2) (xyz 6 3 2))
]

@figure[#:tag "fig:hancock"
        #:caption @elem{The @emph{John Hancock Center}, in
    Chicaco. %Fotografia de User:Cacophony
  }]{
  @authorizedPhoto{JohnHancockCenter}
}

The @emph{John Hancock Center}, in @figref{fig:hancock}, is a good example of a building with a geometry that can
be modelled with a cuboid. In fact this building has a truncated shape with regular base and top. To model it we can
can start by defining the @fn[regular-cuboid-geometry] function parametrized by the base centre point @${P}, the base
length @${c_b} and width @${l_b}, the top base length @${c_t} and width @${l_t} and finally by the height @${h}. For
creating the solid the @lisp[cuboid] function becomes particularly useful, leaving us only with determining the position
each vertex relative to the base point @${P}.

@lispcode[
(define (regular-cuboid-geometry p cb lb ct lt h)
  (cuboid (+xyz p (/ cb -2) (/ lb -2) 0)
          (+xyz p (/ cb +2) (/ lb -2) 0)
          (+xyz p (/ cb +2) (/ lb +2) 0)
          (+xyz p (/ cb -2) (/ lb +2) 0)
          (+xyz p (/ ct -2) (/ lt -2) h)
          (+xyz p (/ ct +2) (/ lt -2) h)
          (+xyz p (/ ct +2) (/ lt +2) h)
          (+xyz p (/ ct -2) (/ lt +2) h)))
]

Using this function, it becomes trivial to create buildings inspired by the shape of the @emph{John Hancock Center},
as presented in @figref{fig:prediosCuboides}.

@figure[#:tag "fig:prediosCuboides"
        #:caption @elem{Buildings inspired by @emph{John Hancock Center}'s cuboid geometry.}]{
  @autoimage{johnHancockCenterA}
}

@figure[#:tag "fig:piloesKarnak"
        #:caption @elem{Pylons of the Karnak Temple.  Illustration by Albert Henry
    Payne.}]{
  @authorizedPhoto{piramides/Karnak}  
}

@question{A pylon is a distinctive Egyptian architectural element, illustrated in @figref{fig:piloesKarnak}. It is
a monumental gateway enclosed by two identical tapering towers on both sides. Each tower is cuboid shaped, with a
rectangular base and top and the remaining sides as trapeziums. Define a conveniently parametrized function capable
of creating a simplified version of a pylon, similar to the one shown in the following image:

@fig{@autoimage{piloneB}}
}


@section{Cylindrical Coordinates}

We have seen in previous sections  some examples of the use of rectangular and polar coordinate systems.
It also became clear that a rightful choice of coordinate system can simplify a geometric problem greatly.

For three-dimensional modelling, besides the rectangular and polar, it is also common to use two other
coordinate systems: @emph{cylindrical coordinates} and @emph{spherical coordinates}.

As we can see in figure @figref{fig:coordenadasCilindricas}, a point in cylindrical coordinates is defined
by a radius @${\rho} radius on the @${z=0} plan, an angle @${\phi} with the @${x} axis and a height @${z}.
It is easy to see that the radius and the angle match the polar coordinates of a point's projection on the @${z} plan.

 @figure[#:tag "fig:coordenadasCilindricas"
        #:caption @elem{Cylindrical coordinates.}]{
  @tex{
\begin{tikzpicture}[scale=3]
\inputtikz{coordenadasCilindricas}
\end{tikzpicture}
}}

From @figref{fig:coordenadasCilindricas} we can easily see that given a point @${(\rho, \phi, z)} in cylindrical
coordinates, that same point in rectangular coordinates would be @$${(\rho \cos \phi, \rho
\sin \phi, z)}.

Likewise, a point in rectangular coordinates @${(x,y,z)} would be represented in in cylindrical coordinates as
@$${(\sqrt{x^2+y^2},\arctan\frac{y}{x}, z)}

These equivalences are assured by the constructor of cylindrical coordinates @lisp[cyl]. Although this function is
pre-defined in Rosetta it is not difficult it is defined as

@lispcode[
(define (cyl ro fi z)
  (xyz (* ro (cos fi))
       (* ro (sin fi))
       z))
]

If we simply want to add to a point a translation in cylindrical coordinates, we can use the @fn[+cyl] function,
which makes use of the @fn[+xyz] function:

@lispcode[
(define (+cyl p ro fi z)
  (+xyz p
        (* ro (cos fi))
        (* ro (sin fi))
        z))
]

@questions[
@question{ Define the selectors @lisp[cyl-rho], @lisp[cyl-phi] and @lisp[cyl-z] which return, respectively,
the components @${\rho}, @${\phi} and @${z} of a point built with the constructor @lisp[cyl]. }

@question{ Define a function @fn[stairs] capable of building a helix staircase. The following image shows three
different examples of helix staircases:

@fig{
    @autoimage{escadasHelice}
 }

It can be seen that a staircase is formed by a central cylinder onto which @${10} cylindrical steps are connected.
For easier testing consider the central cylinder as a radius of @${r}. Each step is equal to the central cylinder,
they measure @${10} times the centre pole radius and are placed at incremental heights. Each is distanced @${h} from
the previous one and, seen from a top view, makes with it an angle @${\alpha}.

The function @fn[stairs] should have as parameters the coordinates of the central cylinder's base, the radius @${r},
the height @${h} and the the angle @${\alpha}. As an example, consider that the stairs in the previous figure were
created with the following expressions:

@lispcode[
(stairs (xyz 0 0 0) 1.0 3 (/ pi 6))
(stairs (xyz 0 40 0) 1.5 5 (/ pi 9))
(stairs (xyz 0 80 0) 0.5 6 (/ pi 8))
]}]


@section{Spherical Coordinates}

As we can see in @figref{fig:coordenadasEsfericas}, a point in spherical coordinates (also called polar) is characterized
by the measurement of the radius @${\rho}, an angle @${\phi} (called @emph{longitude} or @emph{azimuth}) whose projection
onto the @${z=0} plane makes with the @${x} axis an angle @${\psi} (also called @emph{colatitude}, @emph{zenith} or
emph{polar angle}) with the @${z} axis. @footnote{ Colatitude is the complementary angle to the latitude, i.e., the
the difference between the pole (@${\frac{\pi}{2}}) and the latitude.}

@figure[#:tag "fig:coordenadasEsfericas"
        #:caption @elem{Spherical Coordinates}]{
  @tex{
\begin{tikzpicture}[scale=3]
\inputtikz{coordenadasEsfericas}
\end{tikzpicture}
}}

Given a point @${(\rho, \phi, \psi)}, that same point in spherical coordinates is @$${(\rho \sin \psi \cos \phi, \rho \sin \psi \sin \phi,
\rho \cos \psi)}

Likewise a point @${(x,y,z)} in Cartesian coordinates, that same point in spherical coordinates is @$${(\sqrt{x^2+y^2+z^2},\arctan\frac{y}{x},
\arctan\frac{\sqrt{x^2+y^2}}{z})}

As it happens with cylindrical coordinates, the constructors of spherical coordinates @lisp[sph] and @lisp[+sph] are
already pre-defined but it is not difficult to deduce their definitions:

@lispcode[
(define (sph ro fi psi)
  (xyz (* ro (sin psi) (cos fi)) 
       (* ro (sin psi) (sin fi))
       (* ro (cos psi))))

(define (+sph p ro fi psi)
  (+xyz p
        (* ro (sin psi) (cos fi)) 
        (* ro (sin psi) (sin fi))
        (* ro (cos psi))))
]

@questions[
@question{ Define the selectors @lisp[sph-rho], @lisp[sph-phi] and @lisp[sph-psi] which return, respectively, the components
@${\rho}, @${\phi} and @${\psi} of a point built with the constructor @lisp[sph]. }

@question{ The @emph{Mohawk} hairstyle was widely used during the @emph{punk} period. It is defined by styling the hair in the
shape of a crest, as shown below:

@fig{
    @autoimage{moicanos}
}

Define the function @fn[mohawk], with parameters @${P}, @${r}, @${c}, @${\phi} and @${\Delta_\psi}, which creates 9 cones
of length @${c} and base radius @${r}, all centred at point @${P}, leaning with an angle @${\Delta_\psi} between them and
placed along a plane with an angle @${\phi} with the @${XZ} plane.

An example is presented below as a result of the following expressions:

@lispcode[
(mohawk (xyz 0  0 0) 1.0 10 (/ pi 2) (/ pi 6))
(mohawk (xyz 0 15 0) 0.5 15 (/ pi 3) (/ pi 9))
(mohawk (xyz 0 30 0) 1.5 6 (/ pi 4) (/ pi 8))
]
}]


@section{Modelling Doric Columns}

The three-dimensional modelling has the virtue of allowing us to create geometrical entities that are more realistic than mere
agglomerates of lines representing views of that entity. As an example, reconsider the Doric column we introduced back in @secref{sec:ordemDorica}.
In that section we developed a series of functions capable of creating a front view of each of that column's components. Even
though those views are useful it is even more useful to model a column directly as a three-dimensional entity.

In this section we are going to employ some of the most relevant operations for three-dimensional modelling of columns, in
particular truncated cones for shaping the shaft and the Echinus and rectangular box to shape the Abacus.

Before, our "columns" were laid out in the @${XY} axis with them "growing" along the @${Y} axis. Now, only the column's base
will be set on the @${XY} plane: the column's body will grow along the @${Z} axis. Although it would be trivial to employ a
different arrangement of axes, this is the one closest to reality.

Similarly to many other functions in Rosetta, each of the operations to model solids has different ways of being called upon.
For the case of modelling truncated cones - @fn[cone-frustum] - the method to us more convenient is that one that receives the
base centre coordinates, the base radius, height and finally the top radius.

With this in mind we can redefine the operation for creating the column's shaft:

@lispcode[
(define (shaft p h-shaft r-base r-top)
  (cone-frustum p r-base h-shaft r-top))
]

Likewise, the operation for creating the Echinus will become:

@lispcode[
(define (echinus p h-echinus r-base r-top)
  (cone-frustum p r-base a-echinus r-top))
]

Finally, to build the Abacus - the rectangular box at the column's top - we have many ways of specifying it. One way is to
specify the two corners of this box. The other is to specify one of these corners followed by the box's dimensions. For this
example we will employ the second alternative.

@lispcode[
(define (abacus p h-abacus l-abacus)
  (box (+xyz p (/ l-abacus -2) (/ l-abacus -2) 0)
       l-abacus
       l-abacus
       h-abacus))
] 

@questions[
@question{ Implement the @fn[abacus] function but using the other option for creating a rectangular box whit the two corners.}
]

Finally all there is left is to implement the @fn[column] function that, similar to what happened in the bi-dimensional case,
successively invokes the functions @lisp[shaft], @lisp[Echinus] and @lisp[abacus] but now progressively increasing the @${Z} coordinate:

@lispcode[
(define (column p
                h-shaft r-base-shaft
                h-echinus r-base-echinus
                h-abacus l-abacus)
  (shaft p h-shaft r-base-shaft r-base-echinus)
  (echinus (+z p h-shaft) a-echinus r-base-echinus (/ l-abacus 2))
  (abacus (+z p (+ h-shaft h-echinus)) h-abacus l-abacus))
]

With these redefinitions we can now repeat the columns drawn in section @ref{sec:ordemDorica}, and shown in @figref{fig:colunasDoricas},
but now creating a three-dimensional image as presented in @figref{fig:colunasRender0}:

@lispcode[
(column (xyz  0 0 0) 9 0.5 0.4 0.3 0.3 1.0)
(column (xyz  3 0 0) 7 0.5 0.4 0.6 0.6 1.6)
(column (xyz  6 0 0) 9 0.7 0.5 0.3 0.2 1.2)
(column (xyz  9 0 0) 8 0.4 0.3 0.2 0.3 1.0)
(column (xyz 12 0 0) 5 0.5 0.4 0.3 0.1 1.0)
(column (xyz 15 0 0) 6 0.8 0.3 0.2 0.4 1.4)
]


@figure[#:tag "fig:colunasRender0"
        #:caption @elem{Multiple three-dimensional columns.}]{
  @autoimage{colunasA3d}
}


@section{Vitruvian Proportions}

The method for modelling Doric columns that we developed in the previous section allows us to easily build columns, for which we need
only indicate the values for the relevant parameters, such as the shaft's height and base radius, the Echinus' height and base radius and
the Abacus' height and width. Each of these parameters represents a degree of freedom that we can freely vary.

Even though it is logic to think that the more degrees of freedom we have the more flexible the modelling is, the truth is an excessive
number of parameters will often lead to unrealistic models. That phenomenon can be seen in @figref{fig:colunasTortas0} where we show a
a set of columns with randomly chosen parameters.

@figure[#:tag "fig:colunasTortas0"
        #:caption @elem{Three-dimensional columns with randomly chosen parameters. Only one of these columns obeys the canon of the Doric order.}]{
  @autoimage{colunasB3d}
}

In fact, according to the @emph{canons} of the Doric Order, the different parameters that regulate the shape of a column should relate to
each other following a set of well defined proportions. Vitruvius, in his famous architectural treatise, considers that these proportions
derive directly from the proportions of the human body.@footnote{Vitruvius was a Roman writer, architect and engineer during the 1st century
b.C., and the author of the only architectural treatise that has survived from Ancient times.}

@quotation{Because they wanted to raise a temple with columns but did 
not know the adequate proportions, [...], they measured a man's foot and
saw that is was one sixth of his height, so they gave the column a similar
proportion, i.e., they made its height, including the capital, six times
the column's width, measured from the base. Thus the Doric order obtained
its proportion and its beauty from the male human figure.}

Vitruvius characterizes the Doric order in terms of @emph{modules}:

@itemlist[
@item{The columns' width, at the base, shall be two modules and their height, including the capitals, shall be fourteen.

From this we deduce that a module is equal to the column's base radius and that the column's height should be 14 times that radius.
In other terms, the column's base radius should be @${\frac{1}{14}} the column's height.}

@item{The capital's height shall be one module and its width two and one sixth modules.

This implies that the Echinus' height added to the Abacus' height shall be one module, that is, the column's base radius and the
Abacus' width shall be @${2\frac{1}{6}} modules or @${\frac{13}{6}} of the radius. Together with the fact that the column is 14
modules high that implies that the shaft's height shall be 13 times the radius.}

@item{Let the capital's height be divided into three parts, of which one will form the abacus with its cymatium, the second the
Echinus with its annulets, and the third the neck.

This means that the abacus has the height of one third of a module, that is @${\frac{1}{3}} of the base radius and the Echinus will have
the remaining two thirds, which means @${\frac{2}{3}} of the base radius.}
]

These considerations lead us to determine the values of some of the parameters to draw a column, in terms of the
shaft's base radius. In terms of implementing this, that means that the function's parameters will now be local
variables whose value is attributed by applying the proportions established by Vitruvius to the the parameter 
@lisp[r-base-shaft]. The function is thus defined:

@lispcode[
(define (doric-column p r-base-shaft r-base-echinus)
  (define h-shaft (* 13 r-base-shaft))
  (define h-echinus (* 2/3 r-base-shaft))
  (define h-abacus (* 1/3 r-base-shaft))
  (define l-abacus (* 13/6 r-base-shaft))
  (shaft p h-shaft r-base-shaft r-base-echinus)
  (echinus (+z p h-shaft) h-echinus r-base-echinus (/ l-abacus 2))
  (abacus (+z p (+ h-shaft h-echinus)) h-abacus l-abacus))
]
 
or, alternatively:

@lispcode[
(define (doric-column p r-base-shaft r-base-echinus)
  (let ((h-shaft (* 13 r-base-shaft))
        (h-echinus (* 2/3 r-base-shaft))
        (h-abacus (* 1/3 r-base-shaft))
        (l-abacus (* 13/6 r-base-shaft)))
    (shaft p h-shaft r-base-shaft r-base-echinus)
    (echinus (+z p h-shaft) h-echinus r-base-echinus (/ l-abacus 2))
    (abacus (+z p (+ h-shaft a-echinus)) h-abacus l-abacus)))
]

Using this function it is now possible to create columns closer to the Doric proportions (as set by Vitruvius).
@figref{fig:colunasDoricas2} shows the result of evaluating the following expressions:

@lispcode[
(doric-column (xyz  0 0 0) 0.3 0.2)
(doric-column (xyz  3 0 0) 0.5 0.3)
(doric-column (xyz  6 0 0) 0.4 0.2)
(doric-column (xyz  9 0 0) 0.5 0.4)
(doric-column (xyz 12 0 0) 0.5 0.5)
(doric-column (xyz 15 0 0) 0.4 0.7)
]

@figure[#:tag "fig:colunasDoricas2"
        #:caption @elem{Variations of Doric columns according to Vitruvius' proportions.}]{
  @autoimage{colunasC3d}
}

Vitruvius' proportions allowed us to reduce the number of independent parameters for a Doric column to only two:
the shaft's base radius and Echinus' base radius. However, it does not seem right for these parameters to be completely
independent since that allows bizarre columns to be constructed,  where the shaft's top is larger than the base, as it
happens in the rightmost column in @\figref{fig:colunasDoricas2}.

In truth, the characterization of the Doric Order that we presented is incomplete since, for the column's proportions, 
Vitruvius also added:

@quotation{The reduction at the top of a column seems to be regulated
according to the following principles: if a column is less than fifteen
feet, let the base width be divided into six parts and use five of those
and let five of those parts be used to form the top's width. If the column
has between fifteen and twenty feet, let base width be divided into six
and a half parts, and let five and a half of those parts be used for the
upper width of the column. If a column has between twenty to thirty feet
let the base width be divided into seven parts and let the reduced top
measure six of them. A column of thirty to forty feet should be divided at
the base into seven and a half parts, and, at the beginning of the reduction,
it should have six and a half of these parts at the top. Columns of forty
to fifty feet should be divided into eight parts, and reduced to seven of
those at the top of the column under the capital. In the case of higher
columns, let the reduction be determined proportionally, on the same principles.
(In Vitruvius, The Ten Books on Architecture, III book, Chapter 3.1)}

These considerations by Vitruvius allow us to determine the ratio between the top and bottom of a column in terms of its
height in feet. @footnote{ A @emph{Foot} was the fundamental unit of measurement during for several centuries but its
value has changed along the years. The measurement of the international foot is @${304.8} millimetres and was established
in 1958. Before that, many other measurements were used, as the Doric foot of @${324}, the Roman and Ionic feet of @${296}
millimetres, the Athenian foot of @${315} millimetres, the Egyptian and Phoenician feet of @${300} millimetres, etc.}

Let us then consider a function, which we will call @lisp[shaft-top-radius], that receives as parameters the column's base
width and the column's height and returns as the result the width for the column's top width.

A literal translation of Vitruvius' considerations allows us to write:

@lispcode[
(define (shaft-top-radius base-radius height)
  (cond ((< height 15) (* (/ 5.0 6.0) base-radius))
        ...))
]

The previous fragment obviously corresponds to the statement: "if a column is less than fifteen feet, let the base width be 
into six parts and use five of those and let five of those parts be used to form the top's width." In case the column has less
than fifteen feet we skip to the next statement: "If the column has between fifteen and twenty feet, let base width be divided
into six and a half parts, and let five and a half of those parts be used for the upper width of the column". Translating this
last statement we have:


@lispcode[
(define (shaft-top-radius base-radius height)
  (cond ((< height 15) 
         (* (/ 5.0 6.0) base-radius))
        ((and (>= height 15) (< height 20)) 
         (* (/ 5.5 6.5) base-radius))
        ...))
]

A careful analysis of the two previous statements shows that, in reality, we are making excessive tests on the second clause. In
fact, if we can get to the second clause that means the first one is false, i.e., a height is not less than 15 and, therefore,
it is higher or equal to 15. In that case it is useless to test if the height is higher of equal to 15 again. That way we can
simplify the function and write instead:

@lispcode[
(define (shaft-top-radius base-radius height)
  (cond ((< height 15) (* (/ 5.0 6.0) base-radius))
        ((< height 20) (* (/ 5.5 6.5) base-radius))
        ...))
]

Moving on with the translation, leads us to:

@lispcode[
(define (shaft-top-radius base-radius height)
  (cond ((< height 15) (* (/ 5.0 6.0) base-radius))
        ((< height 20) (* (/ 5.5 6.5) base-radius))
        ((< height 30) (* (/ 6.0 7.0) base-radius))
        ((< height 40) (* (/ 6.5 7.5) base-radius))
        ((< height 50) (* (/ 7.0 8.0) base-radius))
        ...))
]

The problem now is that Vitruvius as let the door open to arbitrarily high columns, simply saying: "In the case of higher
columns, let the reduction be determined proportionally, on the same principles". To clearly understand the principles we speak
of let us consider the evolution of the relation between the top and base of the columns which is visible on the side image.
 
The ration between the column's top radius and the base radius is as shown in the side image (something which was also already clear
in the @lisp[shaft-top-radius] function), a sequence like

@$${\frac{5}{6},\,
\frac{5\frac{1}{2}}{6\frac{1}{2}},\, \frac{6}{7},\,
\frac{6\frac{1}{2}}{7\frac{1}{2}},\, \frac{7}{8},\, \cdots{}}

It now becomes obvious that, for higher columns, "the same principles"
Vitruvius speaks of come down to, for each @${10} additional feet,
adding @${\frac{1}{2}} to both the numerator and the
denominator. However, it is important to notice that this principle
should only be applied if the height exceeds @${15} feet, since the
first interval is bigger than the other ones. Thus, we have to handle
columns up to @${15} feet differently and, from there onward, simply
subtract @${20} feet from the height and determine integer division by
@${10} in order to know how many times we need to add @${\frac{1}{2}}
to both the numerator and the denominator of @${\frac{6}{7}}.

It is the "handle differently" for one case and the other which
suggests the need to come up with a selection mechanism: it is
necessary to distinguish between two cases and react to each
accordingly. For Vitruvius' column, if the column has a height @${a}
up to @${15} feet, the ration between the top and the base is
@${r=\frac{5}{6}}; if the height @${a} is not less than @${15} feet,
the ration between the top and the base shall be:

@$${r=\frac{6 + \lfloor\frac{a-20}{10}\rfloor\cdot\frac{1}{2}}{7 + \lfloor\frac{a-20}{10}\rfloor\cdot\frac{1}{2}}}

As an example, let us consider a column with @${43} feet. The integer
division of @${43-20} by @${10} is @${2} so we must add
@${2\cdot{}\frac{1}{2}=1} to the numerator and @${\frac{6}{7}} to the
denominator, and we will get @${\frac{7}{8}=0.875}.

As as for a second example let us consider the proposal made by Adolf
Loos for the headquarters of the Chicago Tribune journal, a @${122}
meter-tall building with the shape of a doric column on top of a large
base. The column alone would be @${85} meters hight.  Taking into
account that a foot, in the Doric Order, measured @${324} millimetres,
the column would have @${85/0.324\approx 262} feet. The integer
division of @${262-20} by @${10} is @${24}. So the ratio between the
top and the base of this hypothetical would then be
@${\frac{6+24/2}{7+24/2}=\frac{18}{19}=0.95}. Because the value is
close to the unit it shows that the column would be practically
cylindrical.

Based on these considerations we can now define a function that, given
an integer representing the column's height in feet, computes the
ration between the top and the base. Beforehand, however, it is
convenient to simplify the formula for columns with heights not
inferior to @${15} feet.  So:

@$${r=\frac{6 + \lfloor\frac{a-20}{10}\rfloor\cdot\frac{1}{2}}{7 + \lfloor\frac{a-20}{10}\rfloor\cdot\frac{1}{2}}= \frac{12 +
\lfloor\frac{a-20}{10}\rfloor}{14 + \lfloor\frac{a-20}{10}\rfloor}= \frac{12 + \lfloor\frac{a}{10}\rfloor - 2}{14 + \lfloor\frac{a}{10}\rfloor - 2}= \frac{10 +
\lfloor\frac{a}{10}\rfloor}{12 + \lfloor\frac{a}{10}\rfloor}}

The function's definition would then be:

@lispcode[
(define (shaft-top-radius base-radius height)
  (if (< height 15)
      (* 5/6 base-radius)
      (let ((divisions (quotient height 10)))
        (* (/ (+ 10 divisions)
              (+ 12 divisions))
           base-radius))))
]

This was the last expression that was missing in order to completely specify the drawing of a Doric column according to Vitruvius in his architectural
treatise. Let us consider that we will supply the coordinates for the column's base centre point and its height. All remaining parameters will be
calculated in terms of these ones. The definition will be: 

@lispcode[
(define (doric-column p height)
  (define r-base-shaft (/ height 14))
  (define r-base-echinus (shaft-top-radius r-base-shaft height))
  (define a-shaft (* 13 r-base-shaft))
  (define a-echinus (* 2/3 r-base-shaft))
  (define a-abacus (* 1/3 r-base-shaft))
  (define l-abacus (* 13/6 r-base-shaft))
  (shaft p a-shaft r-base-shaft r-base-echinus)
  (echinus (+z p a-shaft) a-echinus r-base-echinus (/ l-abacus 2))
  (abacus (+z p (+ a-shaft a-echinus)) a-abacus l-abacus))
]

The following expressions produce the result shown in @figref{fig:colunasDoricas3}: @footnote{Note that the column's height
should be given specified in feet.}

@lispcode[
(doric-column (xy 0 0) 10)
(doric-column (xy 10 0) 15)
(doric-column (xy 20 0) 20)
(doric-column (xy 30 0) 25)
(doric-column (xy 40 0) 30)
(doric-column (xy 50 0) 35)
]

@figure[#:tag "fig:colunasDoricas3"
        #:caption @elem{Column variations according to Vitruvius proportions.}]{
  @autoimage{colunasD3d}
}

Finally it is worth mentioning that the functions @lisp[column] and @lisp[doric-column] represent two extreme cases: the first one models a column
with many degrees of freedom, from the position to the measurements of the shaft, echinus and abacus, whereas the second only allows the position
and height to be given. The function @lisp[doric-column] is in fact a particular case of function @lisp[column] so it can be defined in terms of it:

@lispcode[
(define (doric-column p height)
  (define r-base-shaft (/ height 14))
  (define r-base-echinus (shaft-top-radius r-base-shaft height))
  (define a-shaft (* 13 r-base-shaft))
  (define a-echinus (* 2/3 r-base-shaft))
  (define a-abacus (* 1/3 r-base-shaft))
  (define l-abacus (* 13/6 r-base-shaft))
  (column p
          a-shaft r-base-shaft
          a-echinus r-base-echinus
          a-abacus l-abacus))
]

The functions @lisp[column] and @lisp[doric-column] are also a good example of a modelling strategy. Whenever possible, we should begin by
defining the most general and unconstrained case, contemplating the highest number of degrees of freedom that are reasonable, and only then
should we consider the particular cases, modelled with specific functions but which can naturally resort to the definition of the general case. 

@questions[
@question{A careful look at the @fn[shaft-top-radius] function shows there is a repeated fragment of code, namely the multiplication by the
@lisp[base-radius] parameter. This suggests that it should be possible to come up with an even more compact version of this function. Define it.}]
