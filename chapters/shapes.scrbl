#lang scribble/manual
@(require "../rosetta.rkt")
@setup-math

@title{Constructive Solid Geometry}

@section{Introduction}

So far we have only dealt with curves and simple solids. In this
section we will discuss more complex shapes created from the union,
intersection and subtraction of simpler shapes. As we will see, these
operations are often used in architecture.

@Figref{fig:colunasParede} illustrates the temple of
@emph{Portunus} (also known, erroneously, as temple of @emph{Fortuna Virilis}),
a construction from the first century before Christ
characterized for using columns, not as an eminently structural
element, but as a decorative element. To do so, the
architects considered a @emph{union} between a structural wall and a
set of columns, so that the columns would be embedded in the
wall. This same approach is visible in other monuments such as, for
example, the Coliseum of Rome.

@figure[#:tag "fig:colunasParede"
        #:caption @elem{Temple of Portunus in Rome, Italy.  Photograph by Rickard Lamré.}]{
@authorizedPhoto{Temple_of_Portunus}
}

In the case of the building visible in @figref{fig:nestle},
authored by Rojkind Arquitectos, the architect created an
innovative shape through the @emph{subtraction} of spheres to
parallelepiped shapes.

@figure[#:tag "fig:nestle"
        #:caption @elem{Nestlé Application Group in Querétaro, Mexico.  Photograph by Paúl Rivera - archphoto.}]{
@authorizedPhoto{Nestle2_0903}
}

@Figref{fig:sagradaFamilia} demonstrates a third example. It is the case
of the Sagrada Familia Cathedral, by Catalan architect
Antoni Gaudí. As we will see in section @ref{sec:gaudiColumns}, some
of the columns idealized by Gaudí for this masterpiece are a result of
the @emph{intersection} of twisted prisms.

@figure[#:tag "fig:sagradaFamilia"
        #:caption @elem{Columns of the Sagrada Família Cathedral in Barcelona, Spain.  Photograph by Salvador Busquets Artigas.}]{
@authorizedPhoto{sagradaFamilia/SalvadorBusquetsArtigas2}
}

@section{Constructive Geometry}


@emph{Constructive solid geometry} is one of the most common
techniques used for modelling solids. This approach is based on the
combination of simple solids, such as parallelepipeds, spheres,
pyramids, cylinders, torus, etc.  Each of these solids might be seen
as a set of points in space and their combination is achieved using
set operations such as union, intersection and subtraction of those
sets of points. To simplify, let us refer to the set of points in
space as @emph{region}.

Let us start by considering the operation of @emph{union}. Given
the regions @${R_0} and @${R_1}, their union @${R_0\cup R_1} is the
group of points which either belongs to @${R_0} or to @${R_1}. This operation
is implemented in Rosetta by the @fn[union] function. @Figref{fig:csg2}
shows, on the left side, the union between a cube and a
sphere, produced by the following expression:

@lispcode[
(let ((cube (box (xyz 0 0 0) (xyz 1 1 1)))
      (sphere (sphere (xyz 0 1 1) 0.5)))
  (union cube sphere))
]

Another operation is the @emph{intersection} of regions @${R_0\cap
  R_1}, which produces the group of points that belong 
simultaneously to the sets @${R_0} e @${R_1} and that, obtained in Rosetta
with the @fn[intersection] function. The second image of @figref{fig:csg2}
shows the intersection between a cube and a sphere and
was produced by the expression:

@lispcode[
(let ((cube (box (xyz 2 0 0) (xyz 3 1 1)))
      (sphere (sphere (xyz 2 1 1) 0.5)))
  (intersection cube sphere))
]

Finally, there is also the operation of @emph{subtraction} of regions
@${R_0\setminus R_1} which corresponds to the group of points that
belongs to @${R_0} but do not belong to @${R_1}. Contrary the previous
ones, this operation is not commutative. Thus, subtracting a
sphere to a cube is different to subtracting a cube to a
sphere. This difference is visible in the two images on the right side
of @figref{fig:csg2} which were produced by the expressions:

@lispcode[
(let ((cube (box (xyz 4 0 0) (xyz 5 1 1)))
      (sphere (sphere (xyz 4 1 1) 0.5)))
  (subtraction cube sphere))
]

and

@lispcode[
(let ((cube (box (xyz 6 0 0) (xyz 7 1 1)))
      (shpere (sphere (xyz 6 1 1) 0.5)))
  (subtraction sphere cube))
]

@figure[#:tag "fig:csg2"
        #:caption @elem{Volumes Combination. The image to the left represents the union of a cube with a sphere, the image in the centre represents the intersection of the cube with the sphere and the one on the right represents the subtraction of the cube by the sphere.}]{
@autoimage{csg2}
}

Like other previously discussed functions, such as @fn[line] and
@fn[spline], the functions @fn[union], @fn[intersection], and
@fn[subtraction] receive any number of arguments or, alternatively,
a list with all the arguments. As an example of the use of these
operations, let us consider three cylinders placed along the
@${X}, @${Y} and @${Z} axis. The union of these cylinders can be seen
on the left side of @figref{fig:cylindersUnionIntersection} and
was generated by the expression:

@lispcode[
(union (cylinder (xyz -1 0 0) 1 (xyz 1 0 0))
       (cylinder (xyz 0 -1 0) 1 (xyz 0 1 0))
       (cylinder (xyz 0 0 -1) 1 (xyz 0 0 1)))
]

This object has the characteristic of, when viewed from the top, lateral
elevation and frontal elevation, being reduced to a square. On the
right side of the same figure we have an even more interesting solid,
generated by the intersection of the same three cylinders, producing
an object that, obviously, is not a sphere but, as a sphere, projects
a circle in a top view, frontal and lateral elevation.


@figure[#:tag "fig:cylindersUnionIntersection"
        #:caption @elem{The union and intersection of three cylinders orthogonally arranged.}]{
@autoimage{cilindrosUniaoInterseccao}
}

To better understand the difference between the object built through
the intersection of the cylinders and a real sphere, we can subtract
the sphere to the object. To be able to "peek" inside of the
object, we will subtract a sphere with a (very) slightly bigger radius than
the cylinders:

@lispcode[
(subtraction
  (intersection
    (cylinder (xyz -1 0 0) 1 (xyz 1 0 0))
    (cylinder (xyz 0 -1 0) 1 (xyz 0 1 0))
    (cylinder (xyz 0 0 -1) 1 (xyz 0 0 1)))
  (sphere (xyz 0 0 0) 1.01))
]

The result is presented in @figref{fig:cylindersSphere}.

@figure[#:tag "fig:cylindersSphere" #:caption @elem{Subtraction of a sphere to
the intersection of three cylinders orthogonally arranged. The sphere has a
radius @${1\%} higher to that of the cylinders so we can view the inside.}]{
@autoimage[#:scale 0.5]{cilindrosUniaoInterseccaoOco} }

@questions[
@question{
  We wish to model a stone basin identical to the one presented in the following image:
  
  @fig{@autoimage[#:scale 0.5]{bacia}}

  The parameters relevant for the basin are represented in the frontal
  elevation, plan and lateral elevation presented in the following
  image:

@fig{
@tex{
\begin{tikzpicture}[scale=0.2]
\inputtikz{bacia}
\end{tikzpicture}
}}

Define a function called @fn[basin] that builds a basin identical to
the one on the previous image. }

@question{
  We wish to model a stone bathtub identical to the one presented in the following image:
  
  @fig{@autoimage[#:scale 0.7]{banheira}}

  The parameters relevant for the bathtub are represented in the
  frontal elevation, plan and lateral elevation presented in the
  following image:

@fig{@tex{
\begin{tikzpicture}[scale=0.2]
\inputtikz{banheira}
\end{tikzpicture}
}}

Define a function named @fn[bathtub] which builds a bathtub
identical to the one on the previous image.  } ]

@section{Surfaces}

Until now, we have used Rosetta to create curves, which we normally visualize
in two dimensions, for example in the@${XY} plane, or to create
solids, which we see in three dimensions. Now, we will see that
Rosetta also allows the creation of @emph{surfaces}.

There are many ways for Rosetta to create a surface. Many of the
functions which produce closed curves have a version with the same
parameters but with the difference of producing a surface limited by
those closed curves. Therefore, we have the functions
@fn[surface-circle], @fn[surface-rectangle],
@fn[surface-polygon], and @fn[surface-regular-polygon], which
receive exactly the same arguments as, respectively, the functions
@fn[circle], @fn[rectangle], @fn[polygon], and
@fn[regular-polygon], but which produce surfaces instead of
curves. Besides these ones, there is also the @fn[surface-arc]
function which produces a surface limited by an arch and by the radii
between both extremities and the centre of the
arch. Finally, there is still the @fn[surface] function which
receives a curve or a list of curves and produces a surface limited by
that curve or curves. In fact, we have:

@centered{
@lisp[(surface-circle @ldots)]@${\equiv}@lisp[(surface (circle @ldots))]
}

@centered{
@lisp[(surface-rectangle @ldots)]@${\equiv}@lisp[(surface (rectangle @ldots))]
}

@centered{
@lisp[(surface-polygon @ldots)]@${\equiv}@lisp[(surface (polygon @ldots))]
}

and, in the same way, the equivalences for the remaining functions are
established.

As with solids, surfaces can also be combined with the
operation of union, intersection and subtraction to create more
complex surfaces.  For example, let us consider the following union
between a triangular surface and a circle:

@lispcode[
(union (surface-polygon (xy 0 0) (xy 2 0) (xy 1 1))
       (surface-circle (xy 1 1) 0.5))
]

with the result displayed on the upper left corner of @figref{fig:regionsAlgebra}.

@figure[#:tag "fig:regionsAlgebra"
	#:caption @elem{Combination operations of surfaces applied to a triangular
    surface and a circular surface. On the upper left corner we
    have the union and on the right the intersection.  On
    the lower left corner we have the subtraction of the first
    by the second and on the right the second by the first.}]{
@tex{
  \begin{tikzpicture}[scale=1.5]
    \inputtikz{circuloTriangulo}
  \end{tikzpicture}
}}

If, instead of the union, we had chosen the intersection, then the
result would be the one represented in the upper right corner of
@figref{fig:regionsAlgebra}.  The subtraction of the circle from
the triangle is represented in the lower left corner of @figref{fig:regionsAlgebra}
and, since the subtraction is not a commutative
operation, we can also have the reverse subtraction, shown in the
lower right corner of @figref{fig:regionsAlgebra}.

@subsection{Trefoils, Quatrefoils and Other Foils}

The trefoil is an architectural element which had a widespread use during
the Gothic period.  It is an ornament made up of three tangent circles
arranged around a centre, usually placed on the top of Gothic
windows but can be found in several other places. Besides the
trefoil, Gothic architecture also explores the Quatrefoil, the
Pentafoil and other "foils". In @figref{fig:folioB} we present
several examples of this element, coupled with some of its
derivations.
 
@figure[#:tag "fig:folioB"
        #:caption @elem{Trefoils, quatrefoils, pentafoils and other "foils" in a window of Saint Peter's Cathedral in Exeter, England. Photograph by Chris Last.}]{
@authorizedPhoto{trifolio/ChrisLastCropped-small}
}

In this section we will use the operations which allow the creations and
combination of surfaces to build trefoils, quatrefoils and more generically,
@${n}-foils. For now let us just consider the trefoil.

To determine the parameters of a trefoil let us focus on @figref{fig:trefolioParams}
where we present a "laid down" trefoil. In
that figure we can identify @${R_e} as the external radius of the
trefoil, @${R_f} as the radius of each "leaf" of the trefoil, @${\rho}
the distance from the centre of each leaf to the centre of the trefoil
and @${R_i} the radius of the inner circle of the trefoil. Since the
trefoil divides the circumference into three equal parts, the angle
@${\alpha} will necessarily be half of a third of the
circumference, in other words, @${\alpha=\frac{\pi}{3}}. In the case
of a quatrefoil we will have, obviously, @${\alpha=\frac{\pi}{4}} and
in the general case of a @${n}-foil we will have
@${\alpha=\frac{\pi}{n}}.

@figure[#:tag "fig:trefolioParams"
        #:caption @elem{Parameters of a trefoil.}]{
@tex{
\begin{tikzpicture}[scale=1.5]
\inputtikz{trifolio}
\end{tikzpicture}
}
}

Applying the trigonometric relations we can relate the trefoil parameters with each other:

@$${r_f=\rho\sin\alpha}
@$${r_i=\rho\cos\alpha}
@$${\rho+r_f=r_e}

If we assume that the fundamental parameter is the trefoil's exterior radius, @${R_e}, then we can deduce that:

@$${\rho=\frac{r_e}{1+\sin\frac{\pi}{n}}}
@$${r_f=\frac{r_e}{1+\frac{1}{\sin\frac{\pi}{n}}}}
@$${r_i=r_e\frac{\cos\frac{\pi}{n}}{1+\sin\frac{\pi}{n}}}

From these equations, we can decompose the creation process of a
@${n}-foil as the union of a sequence of circles of radius @${R_f}
arranged circularly with a last central circle of radius @${R_i}.
Transcribing to Racket, we have:

@lispcode[
(define (n-folio p re n)
  (union
    (leafs-folio p re n)
    (circle-interior-folio p re n)))
]

The function @fn[circle-interior-folio] is trivially defined as:

@lispcode[
(define (circle-interior-folio p re n)
  (surface-circle 
    p 
    (* re 
       (/ (cos (/ pi n))
          (+ 1 (sin (/ pi n)))))))
]

For the function @fn[leaves-folio], responsible for creating the
circularly arranged leaves, we will consider applying polar
coordinates. Each leaf (of radius @${R_i}) will be placed in a polar
coordinate determined by the radius @${\rho} and by an angle @${\phi}
that we will recursively increment with
@${\Delta_\phi=\frac{2\pi}{n}}. For that, we will define a new
function to implement this process:

@lispcode[
(define (union-circles p ro fi d-fi rf n)
  (if (= n 0)
    ???
    (union
      (surface-circle (+pol p ro fi) rf)
      (union-circles p ro (+ fi d-fi) d-fi rf (- n 1)))))
]

The problem now is what is what should the function return when
@lisp[n] is zero.  To better understand the problem, let us imagine we
name the circles @lisp[n] as @lispemphi[c]{1}, @lispemphi[c]{2},
@ldots, @lispemphi[c]{n}. Given that, during the recursion, the
function will leave the unions pending, when we reach the stopping
condition we have:

@lispcode[
(union #,(lispemphi c "1")
       (union #,(lispemphi c "2")

                 ...

                       (union #,(lispemphi c "n")
                              ???)))
]

It is now clear that @lisp[???] has to be something that can be used as an
argument for an union and that, besides that, has to not affect the
pending unions. This implies that @lisp[???] has to be the neutral
element of the union @${\varnothing}. It is precisely for that purpose
that Rosetta provides the @fn[empty-shape] function. When invoked,
the @fn[empty-shape] function produces an empty region, i.e., a set
of @emph{empty} of points in space, literally representing an empty
shape that, consequently, is a neutral element in the union of shapes.

Using this function, we can write the full algorithm:

@lispcode[
(define (union-circles p ro fi d-fi rf n)
  (if (= n 0)
    (empty-shape)
    (union
      (surface-circle (+pol p ro fi) rf)
      (union-circles p ro (+ fi d-fi) d-fi rf (- n 1)))))
]

We are now in the condition to define the @fn[leaves-folio] function
which invokes the previous one with the pre-calculated values of
@${\rho}, @${\Delta_\phi} e @${R_f}. To simplify, let us consider an
initial angle @${\phi} of zero.  Hence, we have:

@lispcode[
(define (leaves-folio p re n)
  (union-circles
    p
    (/ re (+ 1 (sin (/ pi n))))
    0
    (/ 2pi n)
    (/ re (+ 1 (/ 1 (sin (/ pi n)))))
    n))
]

Based on these functions, we can create a trefoil next to a
quatrefoil, like we present in @figref{fig:trefoilQuatrefoil},
simply by evaluating the following expressions:

@lispcode[
(n-folio (xy 0 0) 1 3)
]n-folio (xy 2.5 0) 1 4)
}

@figure[#:tag "fig:trefoilQuatrefoil"
        #:caption @elem{A trefoil and a quatrefoil.}]{
  @tex{
  \begin{tikzpicture}[scale=0.6]
    \inputtikz{trifolioQuadrifolio}
  \end{tikzpicture}
}}

Naturally, we can use the function to generate other "foils."
@figref{fig:folios} shows a pentafoil, hexafoil, a heptafoil and an octafoil.

@figure[#:tag "fig:folios"
        #:caption @elem{Foils with increasing number of leaves. From top to bottom and from left to right we have a pentafoil, a hexafoil, a heptafoil, an octofoil, an eneafoil and a decafoil.}]{
  @tex{
  \begin{tikzpicture}[scale=0.6]
    \inputtikz{folios}
  \end{tikzpicture}
}}

Having a generic mechanism for constructing @${n}-foils, it is
tempting to explore its limits, particularly when we reduce the number
of leaves. What will a two leaves foil be?

Unfortunately, when we attempt at creating one of these "bifoils," we
find an error caused by the @fn[circle-interior-folio]
function. The error occurs because, in truth, as the number
of foils decreases, the radius of the interior circle diminishes until
it becomes simply zero. In fact,for @${n=2}, the radius of the interior
circle is @$${r_i=r_e\frac{\cos\frac{\pi}{2}}{1+\sin\frac{\pi}{2}}}
that is, @$${r_i=r_e\frac{0}{2}=0}

It is not hard to fix the problem by inserting a test in the function
@fn[n-folio] that prevents the union of the leaves with the
interior circle when the latter has zero radius. However, that is
not the best option because, from a mathematical point of view, the
algorithm for constructing foils we idealized is still perfectly correct:
it unites an interior circle to a set of leaves. It so happens that
when the interior circle has zero radius it represents an empty set
of points, in other words, an empty shape, and the union of an empty
shape with the circles that correspond to the leaves does not alter the
result.

@figure[#:tag "fig:bifolio"
        #:caption @elem{A "bifoil".}]{
  @tex{
  \begin{tikzpicture}[scale=0.5]
    \inputtikz{bifolio}
  \end{tikzpicture}
}}

Based on the possibility of creating empty shapes, we can now redefine
the function @fn[circle-interior-folio] in a way that, when the
number of leaves is two, the function returns an empty shape:

@lispcode[
(define (circle-interior-folio p re n)
  (if (= n 2)
    (empty-shape)
    (surface-circle p 
                    (* re 
                       (/ (cos (/ pi n))
                          (+ 1 (sin (/ pi n))))))))
]

Applying this redefinition of the function, it is now possible to
evaluate the expression @lisp[(n-folio (xy 0 0) 1 2)] without
generating any error and producing the correct bifoil, visible in
@figref{fig:bifolio}.

With the possibilities of creating bifoils, trefoils, quatrefoils and
@${n}-fólios, we can also legitimately think in the concepts of @emph{unifoil}
and @emph{zerofoil} and try to imagine their form. Unfortunately,
here we bump into mathematical limitations more difficult to overcome:
when the number of foils is one, the radius @${R_i} of the interior
circle becomes negative, losing all geometric sense; when the number
of foils is zero, the situation is equally absurd, as the angle
@${\alpha} becomes an impossibility due to being divided by zero. For
these reasons, the bifoil is the lower limit of the @${n}-foils.


@section{Algebra of Shapes}

In the previous section we saw the necessity of introducing the
concept of empty shape. This necessity becomes more evident when we
compare the operations for combining shapes with algebraic operations, such
as the sum and the product. To begin with, let us
consider a function which adds a list of numbers:

@lispcode[
(define (sum numbers)
  (if (null? numbers)
    0
    (+ (car numbers)
       (sum (cdr numbers)))))
]

As we can see in the previous function, the sum of an @emph{empty}
list of numbers is zero. This makes sense since if there are no numbers to
add, the total is necessarily zero. On the other hand, when we think
that during the recursion some sums remain pending, when it reaches
the stopping condition, it is required it returns a value that does not affect
the pending sums and, once again, it makes sense that this value has
to be zero because that is the neutral element of the sum.

Likewise, if we think of a function that calculates the
union of a list of regions, we should write:

@lispcode[
(define (unions regions)
  (if (null? regions)
    (empty-shape)
    (union (car regions)
           (unions (cdr regions)))))
]

because @lisp[(empty-shape)] is the neutral element of the union of
regions.

let us now consider a function that multiplies a list of numbers,
where we left empty the decision of which the value of the function will be in the
basic case:

@lispcode[
(define (product numbers)
  (if (null? numbers)
    ???
    (* (car numbers)
       (product (cdr numbers)))))
]

Though most people might be tempted to use the number zero as the basic
case value, that would be incorrect as that zero, being the
absorbing element of the product, would be propagated throughout the
sequence of products that would be pending until the basic case,
producing a final result of zero. Thus, it becomes clear that the
correct value for the basic scenario has to be @lisp[1], i.e., the
neutral element of the product:

@lispcode[
(define (product numbers)
  (if (null? numbers)
    1
    (* (car numbers)
       (product (cdr numbers)))))
]

This analysis is crucial so that we can now correctly define a
function to calculate the intersection of a list of regions. Although
it is trivial to write a first draft

@lispcode[
(define (intersections regions)
  (if (null? regions)
    ???
    (intersection (car regions)
                  (intersections (cdr regions)))))
]

it is as evident knowing what to place instead of @lisp[???].
What we know is that that value has to be the neutral element of the
combination operation which, in this case, is the
intersection. Fortunately, it is not hard to conclude that that
neutral element is, necessarily, the @emph{universal shape} @${U},
i.e., the shape that includes all the points of space. That shape
is produced by the function @fn[universal-shape]. This way, we can now
define:

@lispcode[
(define (intersections regions)
  (if (null? regions)
    (universal-shape)
    (intersection (car regions)
                  (intersections (cdr regions)))))
]

It is important we understand that the need for the @fn[empty-shape]
and @fn[universal-shape] functions results from us having to
assure the correct mathematical behaviour of the union, intersection
and subtraction operations of regions. That mathematical behaviour is
dictated by the following functions of the algebra of regions:

@$${R \cup \varnothing = \varnothing \cup R = R}
@$${R \cup U = U \cup R = U}
@$${R \cup R = R}
@$${R \cap \varnothing = \varnothing \cap R = \varnothing}
@$${R \cap U = U \cap R = R}
@$${R \cap R = R}
@$${R \setminus \varnothing = R}
@$${\varnothing \setminus R = \varnothing}
@$${R \setminus R = \varnothing}

@questions[
@question{
  The algebra of regions we elaborated is incomplete for not including
  the @emph{complement} operation of a region. The complement @${R^C} of a
  region @${R} is defined as the subtraction to the universal region @${U}
  of the region @${R}:
  
@$${R^C=U\setminus R}

  The complement operation allows the representation of the concept of @emph{hole}.
  A hole with the shape of the region @${R} is obtained
  simply through @${R^C}. A hole has no obvious geometric
  representation, as it is difficult to imagine a hole without knowing
  which region it is a hole to, but, from the mathematical point of
  view, the concept will make sense if the algebraic operations on the
  regions know how to interpret it.  For example, given the hole
  @${R_1^C}, we can apply it to a region @${R_0} through the
  intersection of the two regions @${R_0\cap R_1^C}. As CAD tools
  do not know how to handle the complement of regions, the operation will
  have to be translated in terms of other operations already known.
  In this case, the subtraction is an obvious choice, as @${R_0\cap
    R_1^C=R_0\setminus R_1}.

  To complete the algebra of holes it is necessary we define the result of
  the union, intersection and subtraction operations when applied to
  holes. Define mathematically those operations, as well as the
  combination between "normal" regions and holes.  }


@question{ Since Rosetta does not implement the concept of complement,
  define a constructor for the complement of regions. The constructor
  should receive the region from which the complement is intended and
  should return an object that symbolically represents the complement
  of the region. Do not forget that the complement of a region's
  complement is the region itself.

  Also define a recognizer of complements that can receive any type
  of object and will only return true for those that represent
  complements of regions.

}

@question{ Define the union, intersection and subtraction operations of regions
  so as to deal with the complement of regions.  }


@question{ Consider the construction of a region composed by a
  recursive union of regular polygons successively smaller and
  centred at the vertices of the polygon immediately bigger as seen
  in the three examples presented in the following figure:

@fig{@tex{
  \begin{tikzpicture}[scale=0.6]
    \inputtikz{poligonosRecursivos0}
  \end{tikzpicture}
}}

As it happened with the @fn[vertices-polygon-regular] function
defined in Section~\ref{sec:polygonsRegular}, each of these polygons
is characterized for being inscribed in a circumference with radius
@${R} centred at a point @${p}, for having a "first" vertex that
makes an angle @${\phi} with the axis @${X} and for having a certain
number of sides @${n}. Furthermore, the construction of regular
polygons is done in a way that each vertex is the centre of a new
identical polygon, but inscribed in a circle whose radius is a
fraction (given by @${\alpha_r}) of the radius @${R}, with this
process being repeated a certain number of levels. For example, in the
previous figure, the image to the left, was created with @${p=(0,0)},
@${R=1}, @${\phi=0}, @${\alpha_r=0.3}, @${n=4} and with number of
levels equal to @${2}. As a whole, the images were produced by the
evaluation of the following expressions:

@lispcode[
(recursive-polygons (xy 0 0) 1 0 0.3 4 2)
(recursive-polygons (xy 3 0) 1 pi/3 0.3 3 3)
(recursive-polygons (xy 6 0) 1 0 0.3 5 4)
]

}

@figure[#:tag "fig:petronas"
        #:caption @elem{Petronas Towers, located at Kuala Lumpur, in Malaysia. photograph by Mel Starrs.}]{
@authorizedPhoto{petronas/einalem}
}

@question{ The Petronas towers were designed by Argentine architect
  César Pelli and were considered the tallest buildings in the world
  from 1998 to 2004.  @Figref{fig:petronas} shows a perspective
  from the top of one of the towers.

  The towers' section, strongly inspired by islamic motifs, is composed
  by two intersecting squares rotated by a quarter of a circle
  and to which circles were added at the intersection points, as
  it can be seen in the constructive diagram.

  @fig{@tex{
    \begin{tikzpicture}[scale=2]
      \inputtikz{petronas}
    \end{tikzpicture}
  }}

  Note that the circles are tangent to the imaginary edges that
  connect the vertices.

  Define a Racket function named @fn[petronas-section] that
  receives the centre of the section and the width @${l} and produces
  the section of the Petronas tower.  Suggestion: use the
  @fn[surface-regular-polygon] and @fn[union-circles] functions
  to generate the relevant regions.  }

@question{ Define the function @fn[petronas-tower] that, from the
  base centre, builds a succession of sections of the Petronas
  tower in order to reproduce the real geometry of Petronas tower, as
  illustrated in the following perspective where the function was
  invoked twice in different positions:

@fig{@autoimage{petronas}}

}


@question{
  Consider the following image:

@fig{@autoimage{coberturaTubos}}

  The image represents a shelter with a cuboid shape built with round
  tubes cut so that the interior space has the shape of a quarter of a
  sphere.  Note that the round tubes have a thickness which is
  @${10\%} of their radius. Also note the relation between the radius
  of the quarter of sphere and that of the cylinders.

  Define a function that builds the shelter from the centre of the
  sphere, the height of the cuboid and the number of tubes to place
  along the height.  }

@question{ Redo the previous exercise but now considering the
  orientation of the tubes visible in the following image:

@fig{@autoimage{coberturaTubos2}}
}

@question{ Redo the previous exercise, but now considering the
  orientation of the tubes as seen in the following image:

@fig{@autoimage{coberturaTubos3}}

}

@question{ Consider the construction of a "sphere" of cones like the
  one presented in the following image:

@fig{@autoimage[#:scale 0.5]{esferaCones0}}

Notice that all the cones have their vertex in the same point and are
oriented so that the centre of the base lies on a virtual sphere.
Notice all the "meridians" and "parallels" have the same number of
cones and also note that the radius of the base of the cones
diminishes as we come closer to the "poles" so that the cones do not
interfere with each other.

Write a Racket program capable of building the "sphere" of cones.

}
@;{
% \INCORPORAR ISTO:
% > A segunda é num exercício resolvido na aula, na função esfera-cones não consigo
% > perceber o primeiro psi (0.1).
% > Segundo a função que eu tinha feito seria
% > (foreach psi (enumera (/ pi 20) (- pi (/ pi 20)) (/ pi 10))
% > (...)
% > de modo a que a soma de todos os psi's desse pi, uma vez que são 10 cones por
% > meridiano e que o primeiro cone do meridiano têm de fazer um angulo psi de (/
% > pi 10) com o primeiro cone do meridiano anterior..
% >
% > A solução da aula é:
% > (foreach psi (enumera 0.1 (- pi 0.1) (/ pi 10))
% >    (foreach fi (enumera (/ pi 10) (* 2 pi) (/ pi 10))
% >      (cone (esf 1 fi psi) (/ (sin psi) 10) (xyz 0 0 0))))
}

@question{ Consider a variation on the "sphere" of cones from the
  previous exercise in which, instead of diminishing the radius of the
  cones' base as we get closer to the poles, we diminish the number of
  cones, as presented in the following image:

@fig[@autoimage[#:scale 0.5]{esferaCones1}]

Write a Racket program able to build this cones "sphere".

}

@question{ Define a Racket function able to create pierced spherical
  shells, as the ones presented below:

@fig{@autoimage{cascasPerfuradas}}

  The function should have as arguments the centre, radius and
  thickness of the spherical shell, and the radius and number of
  perforations to perform along the "equator."  This number should diminish
  as we come closer to the "poles."

}

@question{ Consider the following construction built from a random
  arrangement of pierced spheres:

@fig{@autoimage{cascasEsfericasPerfuradasAleatorias}}

  To model the previous image, define a function that receives two
  points which define the limits of an imaginary volume (parallel to
  the coordinated axes) inside which are located the centres of the
  spheres and the minimum and maximum value of the radius of each
  sphere, the thickness of the shell, the number of spheres to create
  and, finally, the necessary parameters to do the same type of
  perforations on each sphere.

  Note the interior of the construction should be unobstructed, i.e., as
  illustrated in the following section:


@fig{@autoimage{cortecascasEsfericasPerfuradasAleatorias}}

}


@question{ An @emph{impossible cube} (also known as @emph{irrational
    cube}) is a famous optical illusion in which a cube is presented
  in a way that it seems to have some edges in front of the others, in
  an seemingly impossible configuration. The following image shows an
  impossible cube.

@fig[@autoimage[#:scale 0.6]{cuboImpossivel}]

Define a @fn[impossible-cube] function that creates impossible
cubes.

}

@question{ Consider a cube built from an arrangement of smaller cubes,
  as presented in the following image:

  @fig[@autoimage[#:scale 0.6]{cuboOco}]

  As it is easy to see, the smaller cubes each have a third of the side
  of the built cube. Define a function that, from the coordinates of
  one vertex of the cube and the length of the side of the cube to
  build, creates this cube respecting the arrangement of smaller cubes
  that appears in the previous image.

}

@question{ The Menger @emph{sponge} is a known fractal discovered
  by the mathematician Karl Menger. The following image shows several
  stages of building a sponge of Menger.

@fig{@autoimage{esponjaMenger}}

  Like the previous exercise, building a Menger sponge can be done
  through the composition of smaller cubes with the @emph{nuance} of
  replacing the smaller cubes by (sub-)sponges of Menger. Naturally,
  in the case of implementing it on a computer, this infinite
  recursion is impracticable so, because of that, we need to
  establish a stopping condition, from which no more (sub-)sponges of Menger
  are created, but only a cube.

  Define the @fn[menger-sponge] function that receives the
  coordinate of one vertex of the sponge, the sponge dimension and the
  desired level of depth for the recursion.

}

@question{ Consider the following cover made from domes in Roman arch:

@fig{@autoimage{abobadasRomanas}}

  Define a function called @fn[cover-roman-arches] that has as
  parameters the centre of the cover, the radius of the circle that
  circumscribes the cover, the thickness of the cover and the number
  of arches to place, and produces covers as the ones presented above.
  Make sure your function is able to generate the following cover of
  only three arches:

@fig[@autoimage[#:scale 0.7]{abobadaRomana}]
}
]


@section{Slice of Regions}

Besides the union, intersection and subtraction operations, Rosetta
provides the @emph{section} operation, implemented by the function
@fn[slice]. This operation allows the modification of a region through its
section by a plane. The specification of the plane is done through a
point contained in that plane and a normal vector to the plane. The
vector's direction indicates which region we intend to discard.
@Figref{fig:slice} illustrates the slicing of a cube by a
(virtual) plane defined by a point @${P} and by a vector @${\vec{N}}.
This operation's syntax is
@lisp[(slice #,(lispemph region) #,(lispemph P) #,(lispemph N))].

@figure[#:tag "fig:slice"
        #:caption @elem{Sectioning of a solid by a slicing plan defined by a point @${P} belonging to the plane and by a vector @${N} normal to the plane.}]{
@tex{\tdplotsetmaincoords{70}{10}
\begin{tikzpicture}[tdplot_main_coords,scale=1.5]
  \inputtikz{corteCubo}
\end{tikzpicture}}
}

As an example of using this operation let us consider the creation of
a "wedge" (with an opening angle of @${\phi}) of a sphere with a
radius @${R} centred at point @${p}. The wedge is obtained through
two vertical slices on the sphere.

@lispcode[
(define (wedge-sphere p r fi)
  (slice
   (slice
    (sphere p r)
    p (cyl 1 0 0))
   p (cyl 1 fi 0)))
]

@Figref{fig:gomosEsfera} presents wedges with different openings,
generated by the function @fn[wedge-sphere].

@figure[#:tag "fig:gomosEsfera"
        #:caption @elem{Wedges of a sphere. From right to left, the angle between the slicing planes varies from @${0} to @${\pi} in increments of @${\pi/6}.}]{
@autoimage{gomosEsfera}
}

Frequently, the slicing planes we want to employ will have normals parallel to
the coordinates axes. To ease these cases, let us define the
appropriate functions:

@lispcode[
(define (u0) (xyz 0 0 0))
(define (ux) (xyz 1 0 0))
(define (uy) (xyz 0 1 0))
(define (uz) (xyz 0 0 1))
(define (-ux) (xyz -1 0 0))
(define (-uy) (xyz 0 -1 0))
(define (-uz) (xyz 0 0 -1))
]

Using these functions, we can easily create solids with complex shapes.
For example, an eighth of a sphere is trivially obtained through three
slices:

@lispcode[
(slice
  (slice
    (slice
      (sphere (u0) 2)
      (u0) (ux))
    (u0) (uy))
  (u0) (uz))
]

To finish, let us consider the modelling of a tetrahedron. The
tetrahedron is the simplest of the platonic solids and is characterized for
being a polyhedron with four triangular sides. These four sides unite
four vertices that completely specify a tetrahedron. Though Rosetta
provides several operations to model some fundamental solids, such as
the cuboid or the regular pyramid, it does not provide a specific
operation to create tetrahedrons. To implement this operation, we can
produce the tetrahedron through slices on a cuboid that completely
involves the four vertices of the tetrahedron, as we present in @figref{fig:tetraedro}.

@figure[#:tag "fig:tetraedro"
        #:caption @elem{The creation of a tetrahedron by successive slices in an enveloping cuboid.}]{
@autoimage{tetraedroEvol}
}

To specify the involving cuboid, we just need to calculate the maximum
and minimum coordinate values of the tetrahedron's vertices. Then, we
slice the cuboid using the four planes matching the faces of the
tetrahedron, the planes that are defined by the combination of the four vertices
taken three by three. Thus, assuming the tetrahedron has the vertices
@${P_0}, @${P_1}, @${P_2} and @${P_3}, the first slice will be
determined by the plane containing the points @${P_0}, @${P_1} and
@${P_2} and preserving the part which contains @${P_3}, the second
slice will be defined by the points @${P_1}, @${P_2} and @${P_3} and
preserving @${P_0}, the third slice will be defined by the points
@${P_2}, @${P_3} and @${P_0} and preserving @${P_1}, and the fourth by
the points @${P_3}, @${P_0} and @${P_1} and preserving @${P_2}. Each
of these planes will be specified in the slicing operation by a point
and the corresponding normal vector to the plane. For calculating this
normal vector to the plane, we need to use the cross product of two
vectors (implemented by the pre-defined @fn[cross-c] function)
and we have to verify if the normal is pointing towards the point to
preserve, which we can do by verifying the signal of the dot
product (implemented by the pre-determined @fn[dot-c] function)
between the normal vector and the vector that ends at the point to
preserve.

@lispcode[
(define (normal-points p0 p1 p2 p3)
  (let ((v0 (-c p1 p0))
        (v1 (-c p2 p0)))
    (let ((n (cross-c v0 v1)))
      (if (< (dot-c n (-c p3 p0)) 0)
          n
          (*c n -1)))))
]

@lispcode[
(define (tetrahedron p0 p1 p2 p3)
  (define pmin (xyz (min (cx p0) (cx p1) (cx p2) (cx p3))
                    (min (cy p0) (cy p1) (cy p2) (cy p3))
                    (min (cz p0) (cz p1) (cz p2) (cz p3))))
  (define pmax (xyz (max (cx p0) (cx p1) (cx p2) (cx p3))
                    (max (cy p0) (cy p1) (cy p2) (cy p3))
                    (max (cz p0) (cz p1) (cz p2) (cz p3))))
  (define solid (box pmin pmax))
  (set! solid (slice solid p0 (normal-points p0 p1 p2 p3)))
  (set! solid (slice solid p1 (normal-points p1 p2 p3 p0)))
  (set! solid (slice solid p2 (normal-points p2 p3 p0 p1)))
  (set! solid (slice solid p3 (normal-points p3 p0 p1 p2)))
  solid)
]

@Figref{fig:tetraedro} presents the several phases of the
"lapping" process of the cuboid until the tetrahedron is obtained.

@questions[ @question{ In 1609, Johannes Kepler @footnote{Johannes Kepler was a famous
mathematician and astronomer who, among many other contributions,
established the laws of planetary movement.} conceived the
polyhedron illustrated in the following image, which he baptised as
@emph{stella octangula}.   This eight-arm star,
also known as starry octahedron is, in fact, a composition of
two tetrahedrons.

  @fig[@autoimage[#:scale 0.4]{duploTetraedro}]

  Define the function @fn[starry-octahedron] which, given the
  centre of the cube enveloping the octahedron and the length of the
  side of that cube, produces the octahedron imagined by Kepler.

}

@question{ The following image represents successive iterations of
  Sierpiński's pyramid, @footnote{Wacław Sierpiński was a Polish
  mathematician who gave huge contributions to the set theory and
  topology. Sierpiński described a bi-dimensional version of this
  pyramid in 1915.} also called @emph{tetrix}. The Sierpiński pyramid
  is a three-dimensional fractal that can be produced recursively
  through an imaginary tetrahedron with mid points of each edge
  constituting the vertices of sub-Sierpiński pyramids.

@fig[@autoimage{sierpinski}]

Define the @fn[sierpinski] function that, from the coordinates of
the four vertices and the desired level of recursion, creates the
corresponding Sierpiński pyramid.

}

@question{ Consider the spherical cap presented below, characterized by
  the two points @${P_0} and @${P_1} and by the diameter @${d} of the
  cap's base.

  @fig[
@autoimage[#:scale 0.4]{calotaEsferica} @tex{
\begin{tikzpicture}[scale=2.5,rotate=20]
\inputtikz{caloteEsferica}
\end{tikzpicture}
  }]

Define the @fn[spherical-cap] function that receives the points @${P_0} and
@${P_1} and the diameter @${d} of the cap's base. Suggestion: determine the
position and dimension of the sphere and use an appropriate slicing plan.
}
]

@section[#:tag "sec:extrusions"]{Extrusions}

We saw in the previous sections that Rosetta provides a set of
pre-defined solids such as spheres, cuboids, pyramids, etc. Through
the composition of those solids it is possible to model far more
complex shapes but, in any case, these shapes will always be
decomposable into the original basic shapes.

Unfortunately, many of the shapes that our imagination can conceive
are not easy to build from just the compositions of pre-defined
solids. For example, let us consider Kunst- und Ausstellungshalle, a
building designed by the Viennese architect Gustav Peichl and destined
to be a communication and exhibition centre. This building is of a
square shape, but one of its "corners" is cut by a sinusoid,
represented in @figref{fig:kunst}.

@figure[#:tag "fig:kunst"
        #:caption @elem{A detail of the Kunst - und Ausstellungshalle. Photograph by Hanneke Kardol.}]{
@authorizedPhoto{sinus/hanneke}
}


Obviously, if we pretend to model the wavy wall of the Kunst - und
Ausstellungshalle there will not be any pre-defined solid we can use
as a starting point for its construction.

Fortunately, Rosetta provides a set of functionalities which allows us
to easily solve some of these modelling problems. In this section we
will consider two of those functionalities, in particular, the simple
extrusion and the extrusion along a path.

@subsection[#:tag "sec:simpleExtrusion"]{Simple Extrusion}

Mechanically speaking, extrusion is a manufacturing process which
consists in forcing a moldable material through a matrix with the
desired shape in order to produce parts of which section is determined by
that matrix.

In Rosetta, the extrusion is an operation metaphorically identical to the
one used in mechanic manufacturing, though it starts only from the
matrix, which will be "extended" in the desired direction, thus
producing the geometrical shape of which section is the same as the
matrix. The extrusion is provided by the @fn[extrusion] function which
receives, as arguments, the section to extrude and the direction of
the intended extrusion or the height if that direction is vertical. If
the region to extrude is a curve, the extrusion will necessarily
produce a surface. If the region to extrude is a surface, the
extrusion produces a solid. It is important to keep in mind that there
are limitations to the process of extrusion that prevent the
extrusion of excessively complex shapes.

@figure[#:tag "fig:poligonoExtrusao"
        #:caption @elem{An arbitrary polygon.}]{
  @tex{
  \begin{tikzpicture}[scale=0.7]
    \draw (0,2)--(0,5)--(5,3)--(13,3)--(13,6)--(3,6)--(6,7)--(15,7)--(15,2)--(1,2)--(1,0)--(0,2);
  \end{tikzpicture}
}}

As an example, let us consider @figref{fig:poligonoExtrusao} where
an arbitrary polygon set in plane @${XY} is shown. To create a solid
with unitary height from the extrusion of this polygon in in the @${Z}
direction we can use the following expression:

@lispcode[
(extrusion
 (surface-polygon
  (xy 0 2) (xy 0 5) (xy 5 3) (xy 13 3) 
  (xy 13 6) (xy 3 6) (xy 6 7) (xy 15 7) (xy 15 2)
  (xy 1 2) (xy 1 0) (xy 0 2))
 (z 1))
]

The resulting shape is represented on the left in @figref{fig:extrusaoSimples}

@figure[#:tag "fig:extrusaoSimples"
        #:caption @elem{A solid (on the left) and a surface (on the right)
    produced by the extrusion of a polygon.}]{
  @autoimage[#:scale 0.48]{extrusaoSolido} @autoimage[#:scale 0.48]{extrusaoSuperficie}}

In the same figure, on the right, is represented an alternative
extrusion, which instead of producing solids produces surfaces. This
extrusion can be obtained by replacing the previous expression with:

@lispcode[
(extrusion
 (polygon
  (xy 0 2) (xy 0 5) (xy 5 3) (xy 13 3) 
  (xy 13 6) (xy 3 6) (xy 6 7) (xy 15 7) (xy 15 2)
  (xy 1 2) (xy 1 0) (xy 0 2))
 (z 1))
]

Finally, @figref{fig:extrusaoInclinada} illustrates two "flowers,"
the one on the left produced by a set of cylinders with the base set
at the same place and the tops positioned along the surface of a
sphere. The one on the right produced by a set of extrusions from the
same horizontal circular surface, using vectors corresponding to the
tops of the previous cylinders.  As we can see, the extrusions of the
circular surface in directions that are not perpendicular to that
surface produce distorted cylinders.

@figure[#:tag "fig:extrusaoInclinada"
        #:caption @elem{A "flower" (to the left) produced by a set of bent cylinders and (to the right) produced by a set of bent extrusions.}]{
 @autoimage[#:scale 0.48]{florCilindros} 
 @autoimage[#:scale 0.48]{florCirculosExtrudidos}
}


Naturally, it is possible to extrude more complex planar shapes. For
example, for the sinusoidal wall of the Kunst - und Ausstellungshalle, we
can "trace" two parallel sinusoids on the plane @${XY}, we add two line
segments at the extremities to transform them into a surface and,
afterwards, we extrude them to the intended height, as represented in @figref{fig:sinusoideExtrusao}.

@figure[#:tag "fig:sinusoideExtrusao"
        #:caption @elem{Modelling of the Kunst - und Ausstellungshalle's sinusoidal wall.}]{
@autoimage{paredeSeno}
}

This description can be easily translated to Racket:@footnote{The
  @fn[sine-points] function was defined in the section
  @ref{sec:plineVsSpline}.}

@lispcode[
(let ((points-1 (sine-points (xy 0 0) 0 (* 9 pi) 0.2))
      (points-2 (sine-points (xy 0 0.4) 0 (* 9 pi) 0.2)))
  (extrusion
   (surface
     (spline points-1)
     (spline v-2)
     (spline (list (car points-1) (car points-2)))
     (spline (list (last points-1) (last points-2))))
   12))
]

Although the created shape presented in @figref{fig:sinusoideExtrusao} is a
reasonable approximation to the - und Ausstellungshalle's wall, the used sinusoid
is not parametrized enough for us to adapt it to other situations of sinusoidal walls.

For example, in @figref{fig:tucson} we present a detail of the
facade of a building in Tucson, Arizona where the sinusoidal shape is
evident, but where the sinusoid parameters are different from the ones
used for the Kunst - und Ausstellungshalle.

@figure[#:tag "fig:tucson"
        #:caption @elem{Sinusoidal facade of a building in Tucson, Arizona. Photograph by Janet Little.}]{
@authorizedPhoto{sinus/JanetLittle}
}

All these examples show the use of a sinusoidal curve but, truth be
told, each of the sinusoids presented has a different
parametrization. Consequently, if we intend to model all these
curves, we will have to find a formula for the sinusoid curve that is
parametrized enough.


Mathematically, the sinusoid equation is: @$${y(x) = a \sin(\omega x +
  \phi)} where @${a} is the @emph{amplitude} of the sinusoid,
  @${\omega} is the @emph{angular frequency}, i.e., the number of
  cycles by length and @${\phi} is the @emph{phase}, i.e., the "advance"
  or "delay" of the curve towards the @${y}
  axis. @Figref{fig:sinusoide} shows the sinusoidal curve,
  illustrating the meaning of these parameters.

@figure[#:tag "fig:sinusoide"
        #:caption @elem{Sinusoid curve.}]{
  @tex{
  \begin{tikzpicture}[scale=0.6]
  \inputtikz{sinusoide}
  \end{tikzpicture}
}}

The translation of the previous definition to Racket is as follows:

@lispcode[
(define (sinusoid a omega fi x)
  (* a (sin (+ (* omega x) fi))))
]


From the sinusoid function, the next step to model sinusoidal curves
is the definition of a function that produces a list of coordinates
corresponding to the points of the sinusoid curve between the limits
of an interval @${[x_0,x_1]} and separated by an increment of
@${\Delta_x}. Since we might be interested in producing sinusoid curves
of which the "origin" is located at a specific point in space @${p}, it is
convenient that the function which creates the points of the sinusoid
curve includes that point as a parameter as well:

@lispcode[
(define (sinusoid-points p a omega fi x0 x1 dx)
  (if (> x0 x1)
    (list)
    (cons (+xy p x0 (sinusoid a omega fi x0))
          (sinusoid-points p a omega fi (+ x0 dx) x1 dx))))
]

NOte that the generated points are all set on a plane parallel to the
@${XY} plane, with the sinusoid evolving in a direction parallel to
@${X} axis, from a point @${p}. @Figref{fig:sinusoides} shows
three of those curves obtained through the following expressions:

@lispcode[
(spline
  (sinusoid-points (xy 0 0) 0.2 1 0 0 (* 6 pi) 0.4))
(spline 
  (sinusoid-points (xy 0 4) 0.4 1 0 0 (* 6 pi) 0.4))
(spline
  (sinusoid-points (xy 0 8) 0.6 2 0 0 (* 6 pi) 0.2))
]

As can be seen from analysing the figure, the sinusoids have
different amplitudes and different periods.

@figure[#:tag "fig:sinusoides"
        #:caption @elem{Three sinusoidal curves.}]{
  @tex{
  \begin{tikzpicture}[scale=0.6]
    \inputtikz{sinusoides}
  \end{tikzpicture}
}}

To build a sinusoid wall we only need to create two sinusoids with a
distance between them equal to the thickness @${e} of the wall, curves
which we will close to form a region that, afterwards, we will
extrude to get a certain height @${h}:

@lispcode[
(define (sinusoidal-wall p a omega fi x0 x1 dx e h)
  (let ((pts-1 (sinusoid-points p a omega fi x0 x1 dx))
        (pts-2 (sinusoid-points (+y p e) a omega fi x0 x1 dx)))
    (extrusion
     (surface
      (spline pts-1)
      (spline pts-2)
      (spline (list (car pts-1) (car pts-2)))
      (spline (list (last pts-1) (last pts-2))))
     h)))
]


@question{ Consider the extrusion of a random closed curve as
  exemplified in the following figure:

@fig{@autoimage{formasSeccaoAleatoria0}}

Each of the curves is a @emph{spline} produced with points generated by the
@fn[random-radius-circle-points] function defined in the exercise
@ref{question:pontosCirculoRaioAleatorio}. Write an expression capable of
generating a solid similar to the ones presented in the previous image.  }

@question{ Write and expression capable of creating a sequence of floors
  with a random shape but more or less circular, as the ones presented in the
  following example:

@fig{@autoimage{edificioPlantasAleatorias}}

}

@question{ Redo the previous exercise but now in a way that the
  floor sequence to form a semi-sphere, as presented in the
  following image:

@fig{@autoimage{semiEsferaPlantasAleatorias}}

@figure[#:tag "fig:marriot"
        #:caption @elem{Detail of Hotel Marriott in Anaheim.  Photograph by Abbie Brown.}]{
@authorizedPhoto{sinus/AbbieBrown-small}
}
}

@question{ @Figref{fig:marriot} shows a detail of Hotel
  Marriott's facade in Anaheim.  It is easy to see that the balconies
  on the facade are sinusoids with equal amplitude and
  frequency but, ever other floor, the sinusoids present opposite
  phases. Besides that, each balcony corresponds to a sinusoid located at a
  certain height @${z}.

  Create a function called @fn[slab] which, by receiving all the
  parameters needed to generate a sinusoid, creates a rectangular slab
  but with the facade with a sinusoidal shape, as presented in the
  following image:

@fig[@autoimage{laje}]

The function should have the following parameters:

@lispcode[
(define (slab p a omega fi lx dx ly lz)
  ...)
]

which are explained in the following figure:


@fig[@tex{
\begin{tikzpicture}
\inputtikz{esquemaMarriot}
\end{tikzpicture}
}]

Parameter @lisp[dx] represents the increment @${\Delta_x} to consider
for the creation of sinusoid's points. The last parameter @lisp[lz]
represents the thickness of the slab.

As an example, consider the previously represented slab as having been
generated by the invocation:

@lispcode[
(slab (xy 0 0) 1.0 1.0 0 4pi 0.5 2 0.2)
]
}

@question[#:tag "question:handrail"]{ Create a function called
  @fn[handrail] which, by receiving all the necessary parameters to
  generate a sinusoid, creates a handrail with a rectangular section but along
  a sinusoidal curve, as presented in the following image:

@fig[@autoimage{corrimao}]

The function should have the following parameters:

@lispcode[
(define (handrail p a omega fi lx dx l-handrail a-handrail)
  ...)
]

The first parameters are the ones needed to completely specify the
sinusoidal curve. The last two @lisp[l-handrail] and @lisp[a-handrail]
correspond to the width and height of the rectangular section of the handrail.

For example, the previous image could have been generated by the invocation:

@lispcode[
(handrail (xy 0 0) 1.0 1.0 0 4pi 0.5 0.5 0.2)
]

}

@question{ Create a function called @fn[uprights] which, by receiving the
  needed parameters to generate a sinusoid, creates the supporting
  uprights for a handrail with a sinusoid curve, as presented in the
  following image:

@fig[@autoimage{prumos}]

Notice that the uprights have a circular section and, therefore, correspond
to cylinders with a determined height and a determined radius. Also note the
uprights have a certain displacement distance @lisp[dx] between them.

The function should have the following parameters:

@lispcode[
(define (uprights p a omega fi lx dx height radius)
  @ldots)
]

The first parameters are the ones necessary to completely specify the
sinusoid curve. The last two @lisp[height] and @lisp[radius]
correspond to the height and radius of each cylinder.

For example, the previous image could have been generated by the
invocation:

@lispcode[
(uprights (xy 0 0) 1.0 1.0 0 4pi 0.8 1 0.1)
]

}

@question{ Create a function called @fn[guardrail] which, by receiving all the
  necessary parameters to create the handrail and the uprights, creates a
  guard rail, as shown in the following image:

@fig[@autoimage{guarda}]

To simplify, consider that the uprights should have as diameter
the same width as the handrail.

The function should have the following parameters:

@lispcode[
(define (guardrail p a omega fi lx dx 
                a-guardrail l-handrail a-handrail d-upright)
  @ldots)
]

The first parameters are the ones necessary to completely specify the
sinusoid curve. The parameters @lisp[a-guardrail], @lisp[l-handrail],
@lisp[a-handrail] and @lisp[d-uprights] are, respectively, the height of
the guard rail, the width and height of the square section of the handrail
and the horizontal displacement of the uprights.

For example, the previous image could have been generated by the
invocation:

@lispcode[
(guard (xy 0 0) 1.0 1.0 0 4pi 0.5 1 0.1 0.04 0.4)
]
}

@question{ Create a function called @fn[floor] which, by receiving all
  the necessary parameters to create the slab and the guard rail, creates a
  floor, as presented in the following image:

@fig[@autoimage{piso}]

To simplify, consider the guard rail is positioned at the extremity of the
slab. The function @fn[floor] should have the following parameters:

@lispcode[
(define (floor p a omega fi lx dx ly
              a-slab a-guardrail l-handrail a-handrail d-upright)
  @ldots)
]

For example, the previous image could have been generated by the
invocation:

@lispcode[
(floor (xy 0 0) 1.0 1.0 0 2*pi 0.5 2 0.2 1 0.04 0.02 0.4)
]

}

@question{ Create a function called @fn[building] which receives
  all the necessary parameters to create a storey, including the height
  of each storey @lisp[a-storey] and the number of storeys
  @lisp[n-storeys], and creates a building with those storeys, as
  presented in the following image:

@fig[@autoimage{predio0}]

The function @fn[building] should have the following parameters:

@lispcode[
(define (building p a omega fi lx dx ly
                a-slab a-guardrail l-handrail a-handrail d-upright
                a-story n-storeys)
  @ldots)
]

For example, the previous image could have been generated by calling
the following code:

@lispcode[
(building (xy 0 0) 1.0 1.0 0 (* 20 pi) 0.5 20
        0.2 1 0.04 0.02 0.4 4 8)
]
}


@question{ Modify the @fn[building] function to receive an
  additional parameter that represents a phase increment to be
  considered on each floor. Through that parameter it is possible to
  generate buildings with more interesting facades, in which each
  floor has a different sinusoid shape. For example, compare the
  following building with the previous one:

@fig[@autoimage{predio1}]
}


@question{ The following images represent some
  variations around the phase increment. Identify the increment used
  in each image.

  @fig{
    @autoimage[#:scale 0.4]{predio1}
    @autoimage[#:scale 0.4]{predio2}
  }

  @fig{
    @autoimage[#:scale 0.4]{predio3}
    @autoimage[#:scale 0.4]{predio4}
  }
}



@subsection[#:tag "sec:extrusionPath"]{Extrusion Along a Path}

The extrusions done in the previous examples correspond to creating solids
(or surfaces) through the dislocation of a section set at the @${XY}
plane along a certain direction. Through the @fn[sweep] function,
it is possible to generalize the process so that said dislocation is
performed along an arbitrary curve. As it happened with the simple
extrusion, it is important to keep in mind that there are certain
limitation to the extrusion process which prevent the extrusion from
excessively complex forms or along excessively complex paths.

As an example let us consider the creation of a cylindrical tube with a
sinusoidal shape. It is relatively evident that the section to extrude
is a circle, but it is also obvious that the extrusion cannot be
performed over a fixed direction as the result would not have the
intended sinusoidal shape. To solve this problem, instead of extruding
a circle along a line, we can simply dislocate that circle along a
sinusoid, obtaining the image of @figref{fig:cilindroSeno}. This
image was produced by the following expressions:

@figure[#:tag "fig:cilindroSeno"
        #:caption @elem{A cylindrical tube with a sinusoidal shape.}]{
  @autoimage{cilindroSeno}
}

@lispcode[
(let ((section (surface-circle (xy 0 0) 0.4))
      (curve (spline (sine-points (xy 0 0) 0 (* 7 pi) 0.2))))
  (sweep curve section))
]

Both the extrusion along a path (@fn[sweep]), and the simple
extrusion (@fn[extrusion]), allow the creation of countless solids, but it
is important to know which of the operations is more adequate for each
case. As an example, let us again consider the creation of a
sinusoidal wall. In section @ref{sec:simpleExtrusion} we
idealized a process of doing it from a region limited by two
sinusoids, that we extruded vertically, to form a solid
wall. Unfortunately, when the sinusoids used as starting point have
accentuated curvatures, the resulting walls will have a thickness
that is manifestly not uniform, as the image presented in
@figref{fig:paredesSinusoidais} shows.


@figure[#:tag "fig:paredesSinusoidais"
        #:caption @elem{Top view of two overlaying sinusoid walls. The thicker profile was generated from the extrusion of a region limited by two sinusoids. The thinner profile was generated by the extrusion of a rectangular region along a sinusoid.}]{
  @tex{
  \begin{tikzpicture}[scale=0.6]
  \inputtikz{paredesSinusoidais}
  \end{tikzpicture}
}}

To give the wall a uniform thickness, it is preferable we think of the
modelling of the wall as a rectangle, with the height and thickness of the
wall, which moves along the curve that defines the wall. This way,
to create a sinusoid wall, we just need to create the "guide" line
with the shape of a sinusoid along which we will dislocate the rectangular
profile of the wall through:

@lispcode[
(define (sinusoidal-wall p a omega fi x0 x1 dx height width)
  (sweep
    (spline
      (sinusoid-points
        (+xyz p 0 (/ width 2.0) (/ height 2.0))
        a omega fi x0 x1 dx))
    (surface-rectangle p height width)))
]

The previous function easily allows us to create the intended
walls. The following expressions were used to generate the image in
@figref{fig:paredesSinusoide}.

@lispcode[
(sinusoidal-wall (xy 0 0) 0.2 1 0 0 (* 6 pi) 0.4 3 0.2)
(sinusoidal-wall (xy 0 4) 0.4 1 0 0 (* 6 pi) 0.4 2 0.4)
(sinusoidal-wall (xy 0 8) 0.6 2 0 0 (* 6 pi) 0.2 5 0.1)
]

@figure[#:tag "fig:paredesSinusoide"
        #:caption @elem{Three walls with different heights and thickness built over sinusoidal curves.}]{
@autoimage{paredesSinusoide}
}

@question{
Redo exercise @ref{question:handrail} using an extrusion along a path.

}

@question{ Define a function @fn[wall-points] which, given the
  thickness and height of a wall and given a curve described by a list
  of points, builds a wall through dislocating a rectangular section
  with the given thickness and height through a @emph{spline} that
  goes through the given coordinates. If needed, you can use the
  @fn[spline] function which, from a list of points, creates a spline
  that goes through those points.  }

@subsection{Extrusion with Transformation}

The operation of extrusion along a path provided by Rosetta also allows
to apply a transformation to the section as it is being extruded. The
transformation consists in a rotation and in a scale factor, giving
that the rotation becomes particularly useful to model shapes with
torsions. Both rotation and scale factor are optional parameters of
the @fn[sweep] function.

As an example, let us consider the modelling of the columns presented
in the facade of the building represented in @figref{fig:colunasTortas}.

@figure[#:tag "fig:colunasTortas"
        #:caption @elem{"Twisted" columns on the facade of a building in Atlanta. Photograph by Pauly.}]{
  @authorizedPhoto{colunas/pauly}
}

Since the columns presented on the facade correspond to cuboids which suffered
a torsion of @${90^o}, we can simulate this shape trough using a
rectangular section that is rotated the same @${90^o} during the
extrusion process:

@lispcode[
(sweep
  (line (u0) (z 30))
  (surface-polygon (xy -1 -1) (xy 1 -1) (xy 1 1) (xy -1 1))
  pi/2)
]

To better understand the torsion effect, @figref{fig:colunasRodadas} shows the torsion process of columns with a
square section in successive angle increments of @${\frac{\pi}{4}},
from a maximum torsion of @${2\pi} clockwise to the maximum torsion of
@${2\pi} in the opposite direction.


@figure[#:tag "fig:colunasRodadas"
        #:caption @elem{Modelling of the torsion of square section columns. From left to right, the columns were twisted in increments of @${\frac{\pi}{4}} radians.}]{
  @autoimage{colunasRodadas}
}

% \falar de offset e, antes, do join.  Explicar curvas (matematicas)
% http://en.wikipedia.org/wiki/Curve como um objecto unidimensional
% (não necessariamente continuamente diferenciável).


@section[#:tag "sec:Gaudicolumns"]{Gaudí's Columns}

Antoni Gaudí, one of the greatest architects of all times, lead the
Catalan Modernism with a very personal interpretation of the Art Nouveau 
movement, which combined Gothic elements, surrealism elements and
oriental influences. Gaudí searched for his works to have a strong relation
to Nature which was his main source of inspiration. In his
architecture it is frequent to find references to natural elements,
such as surfaces that resemble waves and support structures strongly
inspired in the shape of trees.

In 1883, at the young age of 31, Gaudí started working on the Expiatory
Church of the Holy Family, in Barcelona, where he explored fantastic combinations
of shapes that make this temple, still unfinished,
one of the most singular works in Architecture.

In this section, we will lean over a tiny part of this work, namely,
the columns idealized by Gaudi, which can be partially seen in @figref{fig:colunasGaudi}.

@figure[#:tag "fig:colunasGaudi"
        #:caption @elem{Support columns of the Temple of Sagrada Família in Barcelona.  Photograph by Piqui Cuervo.}]{
@authorizedPhoto{sagradaFamilia/PiquiCuervo}
}


As can be understood from observing the figure, Gaudí imagined
columns in which the shape varies along the column. His goal was to suggest
a stone "forest" and, for that, he modelled columns that branch from
other columns, as if they were branches of a tree, with the union points
between columns resembling "knots" of tree trunks. The variation
of the shapes of the columns is perfectly visible in some of these
"branches," of which the base is squared, but that, at the top, assumes a
nearly circular shape. In other cases, the column terminates with a
section in the shape of a star.

To model these columns, Gaudí gave off two years elaborating a
constructive scheme based on the intersection and union of helical shapes
produced by the "torsion" of prisms @cite{BarriosHernandez2006309}. In
the simplest case, these prisms have a square section and suffer a
torsion of sixteenths of a circle in both ways, i.e.,
@${2\pi/16=\pi/8}.

To generalize Gaudí's approach, let us implement a function that deals
with the general case of a prism with @${n} sides which is twisted at
an arbitrary angle. For that, we will use the function
@fn[surface-regular-polygon] that creates a regular polygon from
the number of vertices @${n}, the polygon centre @${p}, the distance
@${r} of the vertex to the point @${p}, and the angle @${\phi} of the
first vertex with the @${X} axis.

To model the twisted prism, we will create a surface with the shape of
the regular polygon and we will extrude that surface to a height @${h}
twisting it at the same time a certain angle @${\Delta_\phi} and applying
a scale factor of @${e}:

@lispcode[
(define (twisted-prism p r n h fi dfi e)
  (sweep
    (line p (+z p h))
    (surface-regular-polygon n p r fi)
    dfi
    e))
]

To reproduce Gaudí's approach, we will now produce the intersection of
two of these prisms, both twisted at an angle of @${\pi/8}, but the
first one in one direction and the second in the other. To obtain more
realism, let us also apply a scale factor of 0.9, so that the column
narrows as it goes up:

@lispcode[
(intersection
 (twisted-prism (xy 0 0) 1 4 10 0 (/ pi +8) 0.9)
 (twisted-prism (xy 0 0) 1 4 10 0 (/ pi -8) 0.9))
]

The result is the column on the far left side of @figref{fig:colunasGaudiMod}.
The union of these prisms produces another
of the shapes used by Gaudí, visible immediately to the right of the
previous column:

@lispcode[
(union
 (twisted-prism (xy 5 0) 1 4 10 0 (/ pi +8) 0.9)
 (twisted-prism (xy 5 0) 1 4 10 0 (/ pi -8) 0.9))
]

@figure[#:tag "fig:colunasGaudiMod"
        #:caption @elem{Columns obtained by the intersection and union of twisted prisms with a square section.}]{
  @autoimage{colunasGaudi}
}

As done by Gaudí, we can complement the previous columns with its
natural extension, doubling the number of prisms and reducing the
torsion angle to half:

@lispcode[
(intersection
 (twisted-prism (xy 10 0) 1 4 10 0 (/ pi +16) 0.9)
 (twisted-prism (xy 10 0) 1 4 10 0 (/ pi -16) 0.9)
 (twisted-prism (xy 10 0) 1 4 10 (/ pi +4) (/ pi +16) 0.9)
 (twisted-prism (xy 10 0) 1 4 10 (/ pi +4) (/ pi -16) 0.9))
]

@lispcode[
(union
 (twisted-prism (xy 15 0) 1 4 10 0 (/ pi +16) 0.9)
 (twisted-prism (xy 15 0) 1 4 10 0 (/ pi -16) 0.9)
 (twisted-prism (xy 15 0) 1 4 10 (/ pi +4) (/ pi +16) 0.9)
 (twisted-prism (xy 15 0) 1 4 10 (/ pi +4) (/ pi -16) 0.9))
]

The results are visible on the two columns to the right, in @figref{fig:colunasGaudi}.


@section{Revolutions}

A surface of revolution is a surface created by the rotation of a
bi-dimensional curve around an axis. A solid of revolution is a solid
generated by the rotation of a bi-dimensional region around an axis.

With the rotation being a very simple process of creating surfaces and
solids, it is natural that Rosetta provides an operation to do
it. Function @fn[revolve] serves exactly that purpose. The function
@fn[revolve] receives, as arguments, the region to "revolve" and,
optionally, a first point on the rotation axis (by omission, the
origin), a second point on that rotation axis (by omission, a point
above the previous one), the initial angle of revolution (by omission,
zero) and the angle increment for the revolution (by omission,
@${2\cdot\pi}). Naturally, if the increment is omitted or if it is
@${2\cdot\pi} radians, we get a complete revolution.


@subsection{Surfaces of Revolution}

Using these functions it is now easier to create surfaces or solids of
revolution. Let us consider, for example, the @emph{spline} presented
in @figref{fig:splinePreRevolve}, which was created from the
following expression:

@lispcode[
(spline (xyz 0 0 2) (xyz 1 0 -1) (xyz 2 0 1)
        (xyz 3 0 -1) (xyz 4 0 0) (xyz 5 0 -1))
]

@figure[#:tag "fig:splinePreRevolve"
        #:caption @elem{A @emph{spline} used as the base to build a surface of revolution.}]{
@tex{
\begin{tikzpicture}[scale=0.6]
    \inputtikz{spline-revolve0}
  \end{tikzpicture}
}}

Note that the spline is located on the @${XZ} plane and, because of that,
can be used as the base for a surface of which the revolution axis is the
@${Z}axis. To better visualize the "interior" of the surface, let us
consider an opening of @${\frac{2\cdot\pi}{4}}. The corresponding
Racket expression is:

@lispcode[
(revolve
  (spline (xyz 0 0 2) (xyz 1 0 -1) (xyz 2 0 1)
          (xyz 3 0 -1) (xyz 4 0 0) (xyz 5 0 -1))
  (u0)
  (uz)
  (* 1/4 2pi) (* 3/4 2pi))
]

The result of evaluating the previous expression is represented in
@figref{fig:splinePosRevolve}.

@figure[#:tag "fig:splinePosRevolve"
        #:caption @elem{A surface of revolution generated by the @emph{spline} represented in @figref{fig:splinePreRevolve}.}]{@autoimage{splineRevolve}}

Surfaces of revolution are often used in Architecture, namely, to
design domes. The onion shaped dome, for example, is a thoroughly
explored element both in Russian architecture, Mughal architecture and
other architectures influenced by these ones. The image in @figref{fig:cupulaA}
shows the domes of Verkho Saviour Cathedral in
Moscow, Russia.  % Na imagem seguinte,


@figure[#:tag "fig:cupulaA"
        #:caption @elem{Domes of the Verkho Saviour Cathedral in Moscow, Russia. Photograph by Amanda Graham.}]{@authorizedPhoto{cupulas/AmandaGraham}}


Onion shaped domes possess an axial symmetry that allows them to be modelled as
if they were surfaces of revolution. For that, we will define a
function called @fn[dome-revolution] which, from a list of
coordinates belonging to the dome's profile, builds the surface of
revolution that models the dome. To simplify, let us admit that the
dome will be closed at the top and that the top is given by the first
element of the coordinates list. This simplification allows
establishing the surface rotation axis from the first element of the
coordinates list:

@lispcode[
(define (dome-revolution points)
  (revolve
    (spline points)
    (car points)))
]

To test if the function correctly models a dome, we can get some
coordinates from the profile of real domes and use them to invoke the
function. That is precisely what we did in the following expressions:

@lispcode[
(dome-revolution
  (list (xyz 0 0 12)
        (xyz 0 1 8)
        (xyz 0 6 4)
        (xyz 0 6 2)
        (xyz 0 5 0)))
]

@lispcode[
(dome-revolution
  (list (xyz 15 0 9)
        (xyz 15 3 8)
        (xyz 15 7 5)
        (xyz 15 8 3)
        (xyz 15 8 2)
        (xyz 15 7 0)))
]

@lispcode[
(dome-revolution
  (list (xyz 30 0 13)
        (xyz 30 1 10)
        (xyz 30 5 7)
        (xyz 30 5 2)
        (xyz 30 3 0)))
]

which create, from left to right, the surfaces presented in @figref{fig:cupulasLisp}.

@figure[#:tag "fig:cupulasLisp"
        #:caption @elem{Domes generated in Rosetta.}]{
@autoimage{cupulaRevolucao}
}


@question{ Consider the tube with a sinusoidal profile presented in
  the following image.

@autoimage{tuboSinusoidal0}

The tube was produced taking into account the geometrical parameters
described in the following profile:

@fig[@tex{
\begin{tikzpicture}[scale=0.7]
\inputtikz{tuboSinusoidal}
\end{tikzpicture}
}]

Define the function @fn[sinusoidal-tube] which, from the previous
parameters @${P}, @${r}, @${a}, @${omega}, @${fi}, @${lx} and,
finally, from the separation between interpolation points
@${\Delta_x}, generates the intended sinusoidal tube. For example, the
tubes represented in the following image were generated by the
evaluation of the following expressions:

@lispcode[
(sinusoidal-tube (xyz -20 80 0) 20 2.0 0.5        0 60 0.2)
(sinusoidal-tube (xyz   0 30 0)  5 0.2 2.0        0 60 0.1)
(sinusoidal-tube (xyz +30  0 0) 10 1.0 1.0 (/ pi 2) 60 0.2)
]

@fig[@autoimage{tuboSinusoidal1}]
}


@question{ Consider an egg as a generalization of eggs of different
  proportions, as illustrated in the following image:

@fig{@autoimage{ovos3d}}

The egg, though a three-dimensional object, has a section as sketched
in the following image:

@fig[@tex{
\begin{tikzpicture}[scale=0.7]
\inputtikz{ovulo}
\end{tikzpicture}
}]

Define an @fn[egg] function that creates an egg. The function should
receive, only, the coordinates of point @${P}, the @${r_0} and
@${r_1} radii and, finally, the height @${h} of the egg.  }


@subsection{Solids of Revolution}

If, instead of using a curve to produce a surface of revolution, we
use a region, we will produce a solid of revolution. As an example,
let us explore the use of a trefoil to generate an arch. The image on
the left of @figref{fig:trefoilQuatrefoil} shows the region used as a starting
point and @figref{fig:regiaoPosRevolve} shows the created
solid. For this example, we used the following expression:

@figure[#:tag "fig:regiaoPosRevolve"
        #:caption @elem{A solid of revolution generated by the trefoil represented in @figref{fig:trefoilQuatrefoil}.}]{
@autoimage{trifolioRevolucao}
}

@lispcode[
(revolve
  (n-folio (xy 3 0) 1 3)
  (xyz-from-normal (u0) (uy))
  0 pi)
]

Through the combination of extrusions and revolutions, it is possible
to produce very sophisticated models. Let us consider, for example,
the modelling of an arcade of which the columns and arches have sections in
the shape of a n-folio. To model these shapes it is necessary to know
the coordinates @${p} of the centre of the foil used to generate the
column or the arch, the radius @${r_c} of the columns and the @${n_f}
number of foils to use. For the columns it is also necessary to know
the height @${h} and, finally, for the arch it is necessary to know
its radius @${r_a}. The following functions implement those shapes:

@lispcode[
(define (folio-column p rc nf h)
  (extrusion (n-folio p rc nf) h))
]

@lispcode[
(define (folio-arch p rc ra nf)
  (revolve
    (n-folio p rc nf)
    (+x p ra) (+xy p ra 1)
    0 pi))
]

To build the arcade, the simplest way is to define a function that
builds a column and an arch and that, recursively, builds the remaining
arcades until it has no more arcades to build, the case at which it creates
the last column that supports the last arch. The translation of this
algorithm to Racket is:

@lispcode[
(define (folio-arcade p rc nf ra h n)
  (if (= n 0)
    (folio-column p rc nf h)
    (union
     (folio-column p rc nf h)
     (folio-arch (+xyz p 0 0 h) rc ra nf)
     (folio-arcade (+xyz p (* 2 ra) 0 0)
                    rc nf ra h (- n 1)))))
]

The image of @figref{fig:arcadasFolios} presents a perspective on
a series of arcades generated by the following expressions:

@lispcode[
(folio-arcade (xy 0  0) 1  4 5 10 5)
(folio-arcade (xy 0 10) 1  6 5 10 5)
(folio-arcade (xy 0 20) 1  8 5 10 5)
(folio-arcade (xy 0 30) 1 10 5 10 5)
(folio-arcade (xy 0 40) 1 12 5 10 5)
]

@figure[#:tag "fig:arcadasFolios"
        #:caption @elem{Arcades generated by the @fn[folio-arcade] function. From the front to the back, the arcades have quatrefoil, exafoil, octafoil, decafoil and dodecafoil as section.}]{
  @autoimage{arcadasFolio}
}


@question{ We want to create a program capable of generating a
  barrel with the base located on the @${XY} plane. Consider that the
  base of the barrel is closed but the top is open.

  Create a function called @fn[profile-barrel] that receives the
  point @${P}, the radii @${r_0} and @${r_1}, the height @${h} and the
  thickness @${e}, and returns the region that defines the profile of
  the barrel, as presented in the following image:

@fig{@tex{
\begin{tikzpicture}[scale=1]
\inputtikz{perfilBarril}
\end{tikzpicture}
}}
}

@question{ Using the function @fn[profile-barrel], define the
  function @fn[barrel] which, having the same parameters as the
  previous one, creates the three-dimensional barrel. The function
  should have the following form:

@lispcode[
(define (barrel p r0 r1 h e)
  @ldots)
]

As an example, consider that the following image was generated by the
evaluation of the following expressions:

@lispcode[
(barrel (xyz 0 0 0) 1 1.3 4 0.1)
(barrel (xyz 4 0 0) 1 1.5 3 0.3)
(barrel (xyz 8 0 0) 1 0.8 5 0.1)
]

@fig[@autoimage{pipas}]
}


@question{ The barrel created in the previous exercise is excessively
  "modern," not representing the traditional wooden barrels that are
  built from wooden boards put together next to each
  other. The following image shows a few examples of these wooden
  boards, with different dimensions and angular placements:
  
  @fig[@autoimage{pipasTabuas0}]

  Define the function @fn[board-barrel] which, besides the same
  parameters that define a barrel, given the initial rotation angle
  @${\alpha}, that defines where the board begins, and given the angle
  increment @${\Delta_\alpha}, which corresponds to the angular
  amplitude of the board, builds the three-dimensional section of the
  barrel corresponding to the board in question.  }

@question{ Define a function called @fn[boards-barrel] that receives
  as parameters the point @${P}, the radii @${r_0} and @${r_1}, the
  height @${h}, the thickness @${e}, the @${n} number of boards and
  the angular "gap" @${s} between boards, and creates a
  three-dimensional barrel with that number of boards. The following
  image shows some examples of these barrels and was created by the
  evaluation of the following expressions:

@lispcode[
(boards-barrel (xyz 0 0 0) 1 1.3 4 0.1 10 0.05)
(boards-barrel (xyz 4 0 0) 1 1.3 4 0.1  4 0.5)
(boards-barrel (xyz 8 0 0) 1 1.3 4 0.1 20 0.01)
]

@fig[@autoimage{pipasTabuas1}]
}

@section{Sections Interpolation}

Section interpolation, also known as shape morphing, allows the
creation of a three-dimensional object through the interpolation of
planar sections of that object. In Rosetta, the section interpolation is
obtained through the @fn[loft] function or through one of its
variants. These functions operate from an ordered list of the planar
sections (that might be curves or regions) which, to minimize
possible mistakes in the interpolation process, can be optionally
complemented with guidelines. Guidelines are particularly important in
the case of planar sections with rectilinear parts.

In the next sections we will analyse separately each of these forms of
interpolation.

@subsection{Interpolation by Sections}

Let us start with interpolation using only sections. There are two
fundamental ways of doing this interpolation: using a ruled surface
(i.e., surfaces generated by a dislocating line segment)
between each planar section, or using a "smooth" surface (i.e., without sudden
shifts in inclination) that comes closer to several sections.  The
interpolation by ruled surfaces is implemented by the
@fn[loft-ruled] function, while the interpolations by smooth
surfaces is done with the @fn[loft] function.

To compare the effects of these two functions, let us consider the
following expressions that create the interpolation by ruled surfaces
from three circles, represented on the left in @figref{fig:regradaSuave}:

@lispcode[
(let ((circ0 (surface-circle (xyz 0 0 0) 4))
      (circ1 (surface-circle (xyz 0 0 2) 2))
      (circ2 (surface-circle (xyz 0 0 4) 3)))
  (loft-ruled (list circ0 circ1 circ2)))
]

In the same @figref{fig:regradaSuave}, to the right, we can find a
smooth surface interpolation of the same three circles, generated by
the following expressions:

@lispcode[
(let ((circ0 (surface-circle (xyz 0 9 0) 4))
      (circ1 (surface-circle (xyz 0 9 2) 2))
      (circ2 (surface-circle (xyz 0 9 4) 3)))
  (loft (list circ0 circ1 circ2)))
]

@figure[#:tag "fig:regradaSuave"
        #:caption @elem{Surface interpolation of three circles. To the left, using ruled surface interpolation. To the right, using smooth surface interpolation.}]{@autoimage{loftBasico}}

@subsection[#:tag "sec:interpolationGuiding"]{Interpolation with Guiding}

It is relatively obvious that there is an unlimited number of
different surfaces that interpolate two given sections. The algorithms
of interpolation available in Rosetta thus have to take some options to
decide which one is the real interpolation they will produce.
Unfortunately, the taken options are not always the ones the user
wants and it is perfectly possible (and also frequent) that the
produced interpolation does not match the desired outcome, specially
when it is necessary to produce an interpolation between two very
different sections.

This behaviour can be seen in @figref{fig:trianguloHexagono} where
we present the interpolation between an hexagon and a triangle and
where the lack of uniformity of the interpolation is perfectly visible.
@footnote{The exact interpolation that is produced
depends not only on the CAD application that is being used but also
on the version of that application.} The figure on the left was
generated by the following expression:

@lispcode[
(loft
 (list
  (surface-regular-polygon
   6 (xyz 0 0 0) 1.0 0 #t)
  (surface-regular-polygon
   3 (xyz 0 0 5) 0.5 (/ pi 6) #t)))
]

Note, on the left side of @figref{fig:trianguloHexagono}, that one
of the sides of the triangle is directly mapped into one of the sides of
the hexagon, forcing the interpolation to distort the
remaining two sides of the triangle to map the remaining five sides of
the hexagon.

Fortunately, Rosetta offers other ways of doing section interpolation
that minimizes the possibility of errors, in particular, through the
@fn[guided-loft] function. This function receives not only the list
of sections to interpolate but also a list of guiding curves that
restrict the interpolation. This function allows us to solve the problem
reported in @figref{fig:trianguloHexagono} through establishing
guidelines between the relevant vertices of each polygon. These
guidelines will limit Rosetta to generate an interpolation that
contains these lines on its surface and, as a consequence, serve
to control the pairing of points between the sections to interpolate.
For that, we will simply generate three vertices of the hexagon which,
together with the three vertices of the triangle, will allow the creation
of the guidelines:

Since we need to build the guidelines from the polygons' vertices, we
will define a function that, with two lists of points, creates a list
of lines that unite the points two by two:

@lispcode[
(define (guidelines p0s p1s)
  (if (null? p0s)
    (list)
    (cons (line (car p0s) (car p1s))
          (guidelines (cdr p0s) (cdr p1s)))))
]

Next, we will simply generate three vertices of the hexagon that,
together with the three vertices from the triangle, will allow the
creation of the guidelines:

@lispcode[
(guided-loft
 (list
  (surface-regular-polygon
   6 (xyz 3 0 0) 1.0 0 #t)
  (surface-regular-polygon
   3 (xyz 3 0 5) 0.5 (/ pi 6) #t))
 (guidelines
   (regular-polygon-vertices
     3 (xyz 3 0 0) 1.0 0 #t)
   (regular-polygon-vertices
     3 (xyz 3 0 5) 0.5 (/ pi 6) #t)))
]

The evaluation of the previous expression allows Rosetta to produce a
more correct interpolation between the hexagon and the triangle, as
can be seen on the right side of @figref{fig:trianguloHexagono}.

@figure[#:tag "fig:trianguloHexagono"
        #:caption @elem{Top view of the interpolation between an hexagonal section and a triangular section. On the left, the interpolation was performed without guidelines. On the right, the interpolation was performed with guidelines that associate three vertices of the hexagon equally spaced to the three vertices of the triangle.}]{
@autoimage{loftGuiamento0}
}


@question{ Consider the interpolation between two random closed curves
  positioned on different heights as presented in the following
  figure:

@fig{@autoimage{formasSeccaoAleatoria1}}

Each of the curves is a @emph{spline} produced by points generated by
the @fn[random-radius-circle-points] function defined in
exercise @ref{question:pontosCirculoRaioAleatorio}. Write an
expression capable of generating a solid similar to the ones presented in
the previous image.

}
