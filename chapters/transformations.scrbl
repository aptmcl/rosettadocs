#lang scribble/manual
@(require "../rosetta.rkt")
@setup-math

@title{Transformations}

@section[#:tag "sec:transformacoes"]{Introduction}

Until now, all the objects we created had a definitive nature: the
parameters used to build them define univocally the shape of those
objects and all we can do is invoke the functions that create these
objects with different parameters to build different objects.

Although that way of creating objects is powerful enough, there are
alternatives potentially more practical that are based in the
@emph{modification} of previously created objects. That modification
is performed through the operations of @emph{translation},
@emph{rotation}, @emph{reflection} and @emph{scale}.

It is important to note that these operations do not create new
objects, they only affect those to which they are applied. For
example, after applying a translation to an object, this object simply
changes its position.

However, sometimes we want to apply transformations to objects that
produce new objects as a result. For example, we might want to produce a
translation of an object, but leaving the original object in its
original place. One way of doing this will be to apply the translation
operation, not to the original object, but to a copy of it. The
possibility of creating copies of objects can then be used to
distinguish the case where we want the operation to modify an object from
the case where we want the operation to create a new object. For this
purpose, Rosetta provides the @fn[copy-shape] operation that receives
an object as an argument and returns a copy of that object, situated
in the exact same place as the original. Naturally, in the CAD tool two
equal objects will appear overlapping. Typically, the copied object will
subsequently be transformed, for example, by moving it to another
location.

Next we will see which are the transformation operations available in
Rosetta.

@section{Translation}

The @emph{translation} operation moves an object by adding a vector to
all its points, causing all these points to move a certain distance in
a determined direction. The vector components indicate what is the
displacement in relation to each coordinate axis.

To perform this operation, Rosetta provides the @fn[move] operation,
that receives one object and one displacement vector to perform the
translation operation. As an example, we have:


@lispcode[(move (sphere) (xyz 1 2 3))]

We should note that the function returns the object that suffered the
translation to allow its simple linkage with other operations.

It is easy to see that, for the previous example, it is simpler to
immediately specify which is the sphere's centre by writing:

@lispcode[(sphere (xyz 1 2 3))]

However, in more complex cases it can be very advantageous to consider
that the objects are created at the origin and later they suffer a
translation to the desired position.

@figure[#:tag "fig:cruzPapal"
        #:caption @elem{One Papal Cross.}]{
@autoimage[#:scale 0.4]{cruzPapal}}

Let us consider, for example, a @emph{Papal Cross}, defined
by the union of three horizontal cylinders of progressively decreasing
length placed along a vertical cylinder, as can be seen
in @figref{fig:cruzPapal}. It is noteworthy that all the cylinders
have the same radius and that their length and position are in function
of that radius. In terms of proportion, the vertical cylinder of the
Papal Cross has a length equal to @${20} radii, while the horizontal
cylinders have lengths equal to @${14}, @${10} and @${6} radii and
their axis is positioned at a height equal to @${9}, @${13} and @${17}
radii. These proportions are implemented by the following function
from a reference point @${p}:


@def[
(define (papal-cross p radius)
  (union
    (cylinder p
              radius
              (+z p (* 20 radius)))
    (cylinder (+xz p (* -7 radius) (* 9 radius))
              radius
              (+xz p (* +7 radius) (* 9 radius)))
    (cylinder (+xz p (* -5 radius) (* 13 radius))
              radius
              (+xz p (* +5 radius) (* 13 radius)))
    (cylinder (+xz p (* -3 radius) (* 17 radius))
              radius
              (+xz p (* +3 radius) (* 17 radius)))))
]

However, if we assume that the cross is initially positioned at the
origin, we can slightly simplify the previous definition:

@def[
(define (papal-cross radius)
  (union
    (cylinder (u0) 
              radius 
              (z (* 20 radius)))
    (cylinder (xz (* -7 radius) (* 9 radius))
              radius
              (xz (* +7 radius) (* 9 radius)))
    (cylinder (xz (* -5 radius) (* 13 radius))
              radius
              (xz (* +5 radius) (* 13 radius)))
    (cylinder (xz (* -3 radius) (* 17 radius))
              radius
              (xz (* +3 radius) (* 17 radius)))))
]

Naturally, if we want to place the cross at a specific position, for
example, @${1,2}, we should write:

@lispcode[
(move (papal-cross 1) (xy 1 2))
]


@section{Scale}

The @emph{scale} consists in a transformation that increases
or decreases the dimension of an entity without changing its
form. This operation is also called @emph{homothety}. Although it is
conceivable to have a scale operation that modifies each dimension
independently, it is more usual to employ a uniform scale that modifies
simultaneously the three dimensions, affecting them with the same
factor. If the factor is bigger than one, the size increases. If the
fact is smaller than one, the size decreases.

In the case of Rosetta, only a uniform scale operation is provided:
@fn[scale]. It is easy to see that the scale, besides changing the
object's dimension, can also change its position. If that case is not
pretended, the obvious solution is to previously apply a
translation to centre the object an the origin, then apply the scale
operation and finally apply the inverse translation to "return" the
object to its original position.

By using the scale operation, it is possible to further simplify the
previous definition. In truth, because the cross's dimension depends
only on the radius, we can arbitrate a unitary radius which we can later
change through a scale operation. That way, we can write:

@def[
(define (papal-cross)
  (union
   (cylinder (u0) 1 20)
   (cylinder (xz -7 9) 1 (xz +7 9))
   (cylinder (xz -5 13) 1 (xz +5 13))
   (cylinder (xz -3 17) 1 (xz +3 17))))
]

If we want to build a Papal Cross with a determined radius @${r}, for
example, @${r=3}, we need only write:

@lispcode[
(scale (papal-cross) 3)
]


@section{Rotation}

In the @emph{rotation} process, all the object's points move in a
circular motion in turn of a point (two dimensions) or an axis (three
dimensions). In the general case of a three dimensional rotation it is
usual to decompose this into three successive rotations around the
coordinate axes. These rotations around the axes X, Y and Z are
called @emph{main rotations}, by analogy with the concept of
@emph{main axes} that applies to X, Y and Z. Each of these rotations
is performed by the @fn[rotate] function that receives, as
arguments, the object on which the rotation is applied, the rotation
angle, and two points that define the rotation axis. By omission,
those points are the origin and a point above the previous, which
implies that, by omission, the rotation will be performed in relation to the
Z axis.

@Figref{fig:cruzesPapaisTransformadas} illustrates some
combinations of translations, scales and rotations, generated by the
following program:

@lispcode[
(papal-cross)
(move (papal-cross) (x 20))
(move (scale (papal-cross) 1.25) (x 40))
(move (rotate (scale (papal-cross) 1.5) pi/4) (x 60))
(move (rotate (scale (papal-cross) 1.75) pi/4 (u0) (ux)) (x 80))
]

@figure[#:tag "fig:cruzesPapaisTransformadas"
        #:caption @elem{From left to right, a Papal cross of unitary radius placed at the origin, followed by a translation, followed by a scale with translation, followed by a scale with rotation and translation and, finally, scale with rotation around the X axis and translation.}]{
@autoimage{cruzesPapais}
}


@section{Reflection}

In addition to the translation, scale and rotation, Rosetta also
implements the reflection operation, providing for that, the function
@fn[mirror]. This function receives, as arguments, the shape to reflect,
a reflection plane described by a point contained in
the plane and a normal vector to the plane. By omission, the reflection
plane is the XY plane.

As an example, let us consider the hourglass shown in @figref{fig:ampulheta}
that has as parameters the hourglass base centre point,
the hourglass base radius, the hourglass strangulation radius and the
hourglass height.

@figure[#:tag "fig:ampulheta"
        #:caption @elem{A hourglass}]{
@autoimage[#:scale 0.3]{ampulheta}
}

It is not difficult to conceive this shape as the union of two cone
frustums:

@def[
(define (hourglass p rb rc h)
  (union
    (cone-frustum p rb (+z p (/ h 2)) rc)
    (cone-frustum (+z p (/ h 2)) rc (+z p h) rb)))
]

However, it is possible we simplify the previous definition through
the use of the @fn[mirror] operation:

@def[
(define (hourglass p rb rc h)
  (mirror (cone-frustum p rb (+z p (/ h 2)) rc)
          (+z p (/ h 2))))
]

In the following section we will see an example where these operations
will be used for modelling one of the most famous buildings in the
world.

@section[#:tag "sec:operaSydney"]{The Sydney Opera House}

The Sydney Opera House resulted from an international competition
launched in 1957 for the design of a building dedicated to
performances. The winner was the project by Jørn Utzon, a Danish architect
until then little known. His project, even though it did not fully
fulfil the competition requirements, was selected by the famous
architect Eero Saarinen, then a member of the jury, that immediately
considered it as a landmark project. The proposal consisted in a set of
structures, shaped like a shell, capable of accommodating various concert
halls. The result of this final proposal is represented in @figref{fig:operaSydney}.


@figure[#:tag "fig:operaSydney"
        #:caption @elem{The Sydney Opera House. Photograph by Brent Pearson.}]{
@authorizedPhoto{sydney/brenbat}
}


Clearly innovative, Utzon's design was too advanced for the construction
and project design technologies of the time and was
by many considered impossible. In the three years that followed the
project's approval, Utzon, along with the structural engineering team
of the company Ove Arup, tried to find a mathematical formulation for his
hand-drawn shells, having experimented a variety of different
approaches, including parabolic, circular and elliptical shapes, but
all of the solutions had, besides enormous technical
difficulties, very high costs which were completely incompatible with the
approved budget.

In the summer of 1961, Utzon was near the brink of despair and decided
to dismantle the shells' perspex model. However, upon packing the
shells away, he found out they fit almost perfectly inside each other,
which would only be possible if the different shells had the same
curvature at all its points. Now, the surface that has the same curvature
at all its points is, obviously, the sphere, which led Utzon to think that
maybe it would be possible to shape his shells as "cut" triangles on a
sphere's surface. Although this design was not exactly identical to
the original drawings, it had the advantage of being calculable in
computers and, more importantly still, to allow its economic
construction. Ove Arup's collaboration was crucial for Utzon's idea
to be put to practice but the genius idea that
solved the problems of its construction, as well as the original
design, belongs to Utzon. Utzon's idea is explained in a bronze model placed
next to the building of the Opera House, as can be see in @figref{fig:placaUtzon}.


@figure[#:tag "fig:placaUtzon"
        #:caption @elem{Commemorative plate that explains Utzon's idea for modelling the shells. Photography by Matt Prebble, UK,December 2006.}]{
@authorizedPhoto{sydney/prebsta-crop}
}

Unfortunately, construction delays and the ever accumulating
costs, led the government to start questioning the political decision to
build the opera house and forced Utzon to resign when the construction
of the interiors was not yet finished. Utzon was devastated and left
Australia in 1966, never to return. Against the wishes of most
architects, the masterpiece was completed by Peter Hall and inaugurated
in 1973 without a single reference made ​​to Utzon. Unfortunately,
the work of Peter Hall was not at the same level as Utzon's and the
contrast between the stunning exteriors and simple interiors led the
work to be considered a "semi-masterpiece". @footnote{Despite Utzon's
personal drama, this story turned out to have a happy ending: thirty
years later, the Australian government remodelled the Sydney Opera
House to become the true masterpiece recognition that it deserved
and got Utzon, who never got to see his work finished, to accept
directing the work aiming to give back its original appearance.}

In this section, we will model the shells of the Sydney Opera House
following exactly the same solution proposed by Utzon. All the
building shells will be modelled by spherical triangles obtained by
three cuts in a sphere. @Figref{fig:conchasEsfera} shows two
spheres of equal radius from which we cut a triangle in each, so as to
get two of the half-shells that constitute the Sydney Opera House.


@figure[#:tag "fig:conchasEsfera"
        #:caption @elem{Two of the half-shells that constitute the Sydney Opera House overlapping the spheres from where they were obtained by a succession of cuts.}]{
@tex{
\begin{tikzpicture}[scale=0.5]
\inputtikz{conchas}
\end{tikzpicture}}}

To define the sections we can consider that the building will be
aligned in a direction parallel to the @${Y} axis, so the
symmetry axis will therefore corresponds to a section plane of which the normal is the @${X}
axis, as an be seen in @figref{fig:conchasEsferaTopo}. The two
remaining section planes will have normals determined so as to
approximate, as rigorously as we can, the volumes imagined by
Utzon. As is also visible in @figref{fig:conchasEsferaTopo}, each
shell is extracted from a sphere with a fixed radius but centred at
different points. Thus, to model these half-shells we are going to
define a function that, from the centre @${p} of the sphere of radius
@${r} and the normals @${n_1} and @${n_2} of the section planes,
produces a spherical shell with thickness @${e} and with the desired
shape.

@figure[#:tag "fig:conchasEsferaTopo"
        #:caption @elem{Top view of two of the half-shells that constitute the Sydney Opera House overlapping the spheres from which they were obtained by a succession of cuts.}]{
@tex{
\begin{tikzpicture}[scale=0.5]
\inputtikz{conchas2}
\end{tikzpicture}
}}

@def[
(define (half-shell p r e n1 n2)
  (move
   (slice
    (slice
     (slice
      (subtraction
       (sphere (u0) r)
       (sphere (u0) (- r e)))
      (u0) n2)
     (u0) n1)
    (*c (-ux) (cx p)) (-ux))
   p))
]

As an example, @figref{fig:operaRender0} shows a half shell
generated by evaluating the following expression:


@lispcode[
(half-shell (xyz -45 0 0) 75 2 (sph 1 2.0 4.6) (sph 1 1.9 2.6))
]

@figure[#:tag "fig:operaRender0"
        #:caption @elem{A half shell of the Sydney Opera House.}]{
@autoimage[#:scale 0.3]{meiaConchaOperaSydney}
}

To produce a complete shell we only have to apply a reflection
to the half-shell, according to the vertical cut plane:

@def[
(define (shell p r e n1 n2)
  (mirror
   (half-shell p r e n1 n2)
   (u0) (-ux)))
]

@Figref{fig:operaRender1} shows the shell generated by evaluating
the following expression:

@lispcode[
(shell (xyz -45 0 0) 75 2 (sph 1 2.0 4.6) (sph 1 1.9 2.6))
]

@figure[#:tag "fig:operaRender1"
        #:caption @elem{A Sydney Opera House shell.}]{
@autoimage[#:scale 0.4]{conchaOperaSydney}
}

To define the set of shells of a building we will use values that
approximate the produced shells with Utzon's original drawing:

@def[
(define (shells-sydney)
  (union
   (list
    (shell (xyz -45.01 +00.00 +00.00) 75 2
            (sph 1 1.9701 4.5693) (sph 1  1.9125 2.5569))
    (shell (xyz -38.92 -13.41 -18.85) 75 2
            (sph 1 1.9314 4.5902) (sph 1  1.7495 1.9984))
    (shell (xyz -38.69 -23.04 -29.89) 75 2
            (sph 1 1.9324 4.3982) (sph 1  1.5177 1.9373))
    (shell (xyz -58.16 +81.63 -14.32) 75 2
            (sph 1 1.6921 3.9828) (sph 1  1.4156 1.9618))
    (shell (xyz -32.00 +73.00 -05.00) 75 2
            (sph 1 +00.91 4.1888) (sph 1  0.8727 1.3439))
    (shell (xyz -33.00 +44.00 -20.00) 75 2
            (sph 1 +01.27 4.1015) (sph 1  1.1554 1.2217)))))
]

Invoking this function will produce a row of shells presented 
in @figref{fig:operaRender2}.

@figure[#:tag "fig:operaRender2"
        #:caption @elem{A row of shells of the Sydney Opera House.}]{
@autoimage{meiaOperaSydney}
}

In order to facilitate the positioning of the building, we will further include a
rotation around the @${Z} axis, a scaling, and a final translation
applied to each set of shells. Since the building has two sets of shells,
we will call this function @fn[half-opera-sydney]:

@def[
(define (half-opera-sydney rot-z esc trans-x)
  (move
   (scale
    (rotate
     (shells-sydney)
     rot-z)
    esc)
   (x trans-x)))
]

Finally, we can model the entire opera by making a building composed
by a set of shells at a scale of @${1.0} and a second building as a
reduced, rotated and shifted version of the first. The scale used by
Utzon was @${80\%} and the rotation angles and the translations that we
are going to use are those that allow us a high resemblance to the
actual building as it stands:


@def[
(define (opera-sydney)
  (half-opera-sydney +0.1964 1.0 +43)
  (half-opera-sydney -0.1964 0.8 -15))
]

The final result of modelling the shells is shown in @figref{fig:operaRender4}.

@figure[#:tag "fig:operaRender4"
        #:caption @elem{The complete model of shells of the Sydney Opera House.}]{
@autoimage{operaSydney}
}

@questions[
@question{ Define a function that creates a @emph{link} of a
  @emph{chain} such as the one shown to the left of the following image.

  @fig[@autoimage{elo}]

  To simplify the modelling process, consider the link as decomposable
  into @emph{fourths} of a link, as it is shown to the right of the
  previous image. This way, it will be enough to define a function that creates
  a fourth of a link (at the cost of a quarter of a torus and a
  half-cylinder), and then apply a double reflection in the @${X} and
  @${Y} axes to compose the complete link.

  The function should receive, as parameters, the radius @${r_e} of
  the link, the radius @${r_i} of the "wire" and length @${l} between
  the semi-circles, such as shown in the following diagram.
  
  @fig[@tex{
  \begin{tikzpicture}[scale=1.7]
    \inputtikz{elo}
    \end{tikzpicture}}]
}

@question{ Define a function capable of creating @emph{chains} such as those
  presented below:

  @fig[@autoimage{corrente}]
Note that, as the chain is constructed, the links suffer successive rotations around the @${X} axis.
}

@question{ Define a function to create closed chains such as the one
  presented bellow:

  @fig[@autoimage{correnteFechada}]

  Note that, as the chain is constructed, the links suffer successive
  rotations around the @${X} axis.

}
]
