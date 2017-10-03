#lang scribble/manual
@(require "../rosetta.rkt")
@setup-math

@title{Operations}

This section describes the data types and operations available in
Rosetta.

@defmodule[rosetta/tikz]

@section{Types}

Rosetta explores several different types of entities, from numbers to geometric shapes.

@defidform[Real]{
  The type of real numbers such as @racket[1], @racket[2/3], and @racket[1.5].
}

@defidform[Integer]{
  The type of integers numbers such as @racket[1], @racket[2], and @racket[12345].
}

@defidform[Loc]{ The type of locations (also known as
  @emph{positions}. The members of this type are particular locations
  in space, such as @racket[(xy 1 2)], @racket[(pol 1 pi)], and
  @racket[(xyz 3 2 1)]. Locations are always specified in relation to
  some specific coordinate space, frequently, implicitly defined.  }

@defidform[Vec]{ The type of vectors. The members of this type are
  particular vectors in space, such as @racket[(vxy 1 2)],
  @racket[(vpol 1 pi)], and @racket[(vxyz 3 2 1)]. Vectors are always
  specified in relation to some specific coordinate space,
  frequently, implicitly defined.
}

@defidform[Cs]{
  The type of coordinate space. The members of this type are particular coordinate spaces.
}

@defidform[Shape]{
  The type of shapes. The members of this type includes the values produced by functions such as @racket[circle], @racket[line], and @racket[sphere].
}

@section{Coordinate Space}

In Rosetta, every location references a particular coordinate space,
by default, the @emph{World Coordinate Space}.

@defthing[world-cs Cs]{
  The world coordinate space.
}

Operations that create locations, such as @fn[xyz] or @fn[pol], have
an optional parameter for specifying the coordinate space that should
be used. If omitted, that parameter defaults to the value of
@racket[(current-cs)].

@defparam[current-cs Cs Cs
          #:value world-cs]{
  A parameter that defines the current coordinate space for operations that create locations. 
  Default value is the world coordinate space.
}

@defform[(with-cs expression body ...)]{ Temporarily switches
  @racket[(current-cs)] to be the value of @racket[expression], evaluates
  @racket[body] and restores the previous value of @racket[(current-cs)],
  returning the value of the last expression in @racket[body].  }

@section{Locations}

Rosetta provides a large number of operations for specifying
locations. Most of the operations that create locations accept a
coordinate space as an optional argument, so that the location is
relative to this coordinate space.

@defproc[(xyz [x Real] [y Real] [z Real] [cs Cs (current-cs)]) Loc]{
  Returns a location in the coordinate space @racket[cs] defined by
  the Cartesian coordinates @racket[x], @racket[y], and @racket[z].
}

Example:

@incremental[
(xyz 1 2 3)
(xyz (* 2 3) (+ 4 1) (- 6 2))
]

@defproc[(cx [p Loc]) Real]{
  Returns the first Cartesian coordinate of location @racket[p].
}

Example:
@incremental[
(cx (xyz 1 2 3))
(cx (xyz (* 2 3) (+ 4 1) (- 6 2)))
]

@defproc[(cy [p Loc]) Real]{
  Returns the second Cartesian coordinate of location @racket[p].
}

Example:
@incremental[
(cy (xyz 1 2 3))
(cy (xyz (* 2 3) (+ 4 1) (- 6 2)))
]

@defproc[(cz [p Loc]) Real]{
  Returns the third Cartesian coordinate of location @racket[p].
}

Example:
@incremental[
(cz (xyz 1 2 3))
(cz (xyz (* 2 3) (+ 4 1) (- 6 2)))
]

@defproc[(x [x Real 1] [cs Cs (current-cs)]) Loc]{
  Returns a location in the coordinate space @racket[cs] defined by the Cartesian coordinates @racket[x], @racket[0], and @racket[0], that is, on the @${X} axis.
}

Example:
@incremental[
(x 5)
(x)
]

@defproc[(y [y Real 1] [cs Cs (current-cs)]) Real]{
  Returns a location in the coordinate space @racket[cs] defined by the Cartesian coordinates @racket[0], @racket[y], and @racket[0], that is, on the @${Y} axis.
}

Example:
@incremental[
(y 3)
(y)
]

@defproc[(z [z Real 1] [cs Cs (current-cs)]) Real]{
  Returns a location in the coordinate space @racket[cs] defined by the Cartesian coordinates @racket[0], @racket[0], and @racket[z], that is, on the @${Z} axis.
}

Example:
@incremental[
(z 10)
(z)
]

@defproc[(xy [x Real] [y Real] [cs Cs (current-cs)]) Real]{
  Returns a location in the coordinate space @racket[cs] defined by the Cartesian coordinates @racket[x], @racket[y], and @racket[0], that is, on the @${XY} plane.
}

Example:
@incremental[
(xy 2 5)
(xy 0 0)
]

@defproc[(xz [x Real] [z Real] [cs Cs (current-cs)]) Real]{
  Returns a location in the coordinate space @racket[cs] defined by the Cartesian coordinates @racket[x], @racket[0], and @racket[z], that is, on the @${XZ} plane.
}

Example:
@incremental[
(xz 1 8)
]

@defproc[(yz [y Real] [z Real] [cs Cs (current-cs)]) Real]{
  Returns a location in the coordinate space @racket[cs] defined by the Cartesian coordinates @racket[0], @racket[y], and @racket[z], that is, on the @${YZ} plane.
}

Example:
@incremental[
(yz 3 7)
]

@defproc[(+xyz [p Loc] [dx Real] [dy Real] [dz Real]) Loc]{
  Returns a
  location in the coordinate space of location @racket[p] defined by
  adding to location @racket[p] the translation vector defined by the
  Cartesian coordinates @racket[dx], @racket[dy], and @racket[dz].  }

Example:
@incremental[
(+xyz (xyz 0 0 0) 1 2 3)
(+xyz (xyz 1 2 3) 4 5 6)
]

@defproc[(+x [p Loc] [dx Real]) Loc]{
  Returns a location in the coordinate space of location @racket[p] defined by adding to location
  @racket[p] the translation vector defined by the Cartesian
  coordinates @racket[dx], @racket[0], and @racket[0].
}

Example:
@incremental[
(+x (xyz 0 0 0) 5)
]

@defproc[(+y [p Loc] [dy Real]) Loc]{
  Returns a location in the coordinate space of location @racket[p] defined by adding to location
  @racket[p] the translation vector defined by the Cartesian
  coordinates @racket[0], @racket[dy], and @racket[0].
}

Example:
@incremental[
(+y (xyz 1 2 3) 5)
]

@defproc[(+z [p Loc] [dz Real]) Loc]{
  Returns a location in the coordinate space of location @racket[p] defined by adding to location
  @racket[p] the translation vector defined by the Cartesian
  coordinates @racket[0], @racket[0], and @racket[dz].
}

Example:
@incremental[
(+z (xyz 1 2 3) 3)
]

@defproc[(+xy [p Loc] [dx Real] [dy Real]) Loc]{
  Returns a location in the coordinate space of location @racket[p] defined by adding to location
  @racket[p] the translation vector defined by the Cartesian
  coordinates @racket[dx], @racket[dy], and @racket[0].
}

Example:
@incremental[
(+xy (xyz 1 2 3) 1 3)
]

@defproc[(+xz [p Loc] [dx Real] [dz Real]) Loc]{
  Returns a location in the coordinate space of location @racket[p] defined by adding to location
  @racket[p] the translation vector defined by the Cartesian
  coordinates @racket[dx], @racket[0], and @racket[dz].
}

Example:
@incremental[
(+xz (xyz 1 2 3) 1 5)
]

@defproc[(+yz [p Loc] [dy Real] [dz Real]) Loc]{
  Returns a location in the coordinate space of location @racket[p] defined by adding to location
  @racket[p] the translation vector defined by the Cartesian
  coordinates @racket[0], @racket[dy], and @racket[dz].
}

Example:
@incremental[
(+yz (xyz 2 1 5) 1 2)
]

@defproc[(pol [rho Real] [phi Real] [cs Cs (current-cs)]) Loc]{
  Returns a location in
  the coordinate space @racket[cs] defined by the polar coordinates @racket[rho] and @racket[phi],
  corresponding to the cylindrical coordinates @racket[rho],
  @racket[phi], and @racket[0].
}

Example:
@incremental[
(pol 1 0)
(pol (sqrt 2) (/ pi 4))
(pol 1 (/ pi 2))
(pol 1 pi)
]

@defproc[(pol-rho [p Loc]) Real]{
  Returns the first polar coordinate of location @racket[p].
}

Example:
@incremental[
(pol-rho (pol 10 (/ pi 3)))
(pol-rho (xyz 1 2 3))
]

@defproc[(pol-phi [p Loc]) Real]{
  Returns the second polar coordinate of location @racket[p].
}

Example:
@incremental[
(pol-phi (pol 10 (/ pi 3)))
(pol-phi (xyz 1 2 3))
]

@defproc[(+pol [p Loc] [drho Real] [dphi Real]) Loc]{
  Returns a location in the coordinate space of location @racket[p] defined by adding to location
  @racket[p] the translation vector defined by the polar
  coordinates @racket[drho] and @racket[dphi].
}

Example:
@incremental[
(+pol (xy 1 2) (sqrt 2) (/ pi 4))
(+pol (xy 1 2) 1 0)
(+pol (xy 1 2) 1 (/ pi 2))
]

@defproc[(cyl [rho Real] [phi Real] [z Real] [cs Cs (current-cs)]) Loc]{
  Returns a location in
  the coordinate space @racket[cs] defined by the cylindrical coordinates @racket[rho],
  @racket[phi], and @racket[z].
}

Example:
@incremental[
(cyl 5 0 10)
]

@defproc[(cyl-rho [p Loc]) Real]{
  Returns the first cylindrical coordinate of location @racket[p].
}

Example:
@incremental[
(cyl-rho (cyl 10 pi 3))
(cyl-rho (xyz 1 2 3))
]

@defproc[(cyl-phi [p Loc]) Real]{
  Returns the second cylindrical coordinate of location @racket[p].
}

Example:
@incremental[
(cyl-phi (cyl 10 pi 3))
(cyl-phi (xyz 1 2 3))
]

@defproc[(cyl-z [p Loc]) Real]{
  Returns the third cylindrical coordinate of location @racket[p].
}

Example:
@incremental[
(cyl-z (cyl 10 pi 3))
(cyl-z (xyz 1 2 3))
]

@defproc[(+cyl [p Loc] [drho Real] [dphi Real] [dz Real]) Loc]{
  Returns a location in the coordinate space of location @racket[p] defined by adding to location
  @racket[p] the translation vector defined by the cylindrical
  coordinates @racket[drho], @racket[dphi], and @racket[dz].
}

Example:
@incremental[
(+cyl (xyz 1 2 3) 3 pi/6 5)
]

@defproc[(sph [rho Real] [phi Real] [psi Real] [cs Cs (current-cs)]) Loc]{
  Returns a location in
  the coordinate space @racket[cs] defined by the spherical coordinates @racket[rho],
  @racket[phi], and @racket[psi].
}

Example:
@incremental[
(sph (sqrt 3) pi/4 (atan (sqrt 2)))
]

@defproc[(sph-rho [p Loc]) Real]{
  Returns the first spherical coordinate of location @racket[p].
}

Example:
@incremental[
(sph-rho (sph 10 pi pi/2))
(sph-rho (xyz 1 2 3))
]

@defproc[(sph-phi [p Loc]) Real]{
  Returns the second spherical coordinate of location @racket[p].
}

Example:
@incremental[
(sph-phi (sph 10 pi pi/2))
(sph-phi (xyz 1 2 3))
]

@defproc[(sph-psi [p Loc]) Real]{
  Returns the third spherical coordinate of location @racket[p].
}

Example:
@incremental[
(sph-psi (sph 10 pi pi/2))
(sph-psi (xyz 1 2 3))
]

@defproc[(+sph [p Loc] [drho Real] [dphi Real] [dpsi Real]) Loc]{
  Returns a location in the coordinate space of location @racket[p] defined by adding to location
  @racket[p] the translation vector defined by the spherical
  coordinates @racket[drho], @racket[dphi], and @racket[dpsi].
}

Example:
@incremental[
(+sph (xyz 1 2 3) 4 pi/4 pi/3)
]

@section{Vectors}

Vectors are used in Rosetta to specify entities that have a direction
and a magnitude. Most of the operations that create vectors accept a
coordinate space as an optional argument, so that the vector is
related to this coordinate space.

@defproc[(vxyz [x Real] [y Real] [z Real] [cs Cs (current-cs)]) Loc]{
  Returns a vector in the coordinate space @racket[cs] defined by
  the Cartesian coordinates @racket[x], @racket[y], and @racket[z].
}

Example:

@incremental[
(vxyz 1 2 3)
(vxyz (* 2 3) (+ 4 1) (- 6 2))
]

@defproc[(cx [v Loc]) Real]{
  Returns the first Cartesian coordinate of vector @racket[v].
}

Example:
@incremental[
(cx (vxyz 1 2 3))
(cx (vxyz (* 2 3) (+ 4 1) (- 6 2)))
]

@defproc[(cy [v Loc]) Real]{
  Returns the second Cartesian coordinate of location @racket[v].
}

Example:
@incremental[
(cy (vxyz 1 2 3))
(cy (vxyz (* 2 3) (+ 4 1) (- 6 2)))
]

@defproc[(cz [v Loc]) Real]{
  Returns the third Cartesian coordinate of location @racket[v].
}

Example:
@incremental[
(cz (vxyz 1 2 3))
(cz (vxyz (* 2 3) (+ 4 1) (- 6 2)))
]

@defproc[(vx [x Real 1] [cs Cs (current-cs)]) Loc]{
  Returns a vector in the coordinate space @racket[cs] defined by the Cartesian coordinates @racket[x], @racket[0], and @racket[0], that is, on the @${X} axis.
}

Example:
@incremental[
(vx 5)
(vx)
]

@defproc[(vy [y Real 1] [cs Cs (current-cs)]) Real]{
  Returns a vector in the coordinate space @racket[cs] defined by the Cartesian coordinates @racket[0], @racket[y], and @racket[0], that is, on the @${Y} axis.
}

Example:
@incremental[
(vy 3)
(vy)
]

@defproc[(vz [z Real 1] [cs Cs (current-cs)]) Real]{
  Returns a vector in the coordinate space @racket[cs] defined by the Cartesian coordinates @racket[0], @racket[0], and @racket[z], that is, on the @${Z} axis.
}

Example:
@incremental[
(vz 10)
(vz)
]

@defproc[(vxy [x Real] [y Real] [cs Cs (current-cs)]) Real]{
  Returns a vector in the coordinate space @racket[cs] defined by the Cartesian coordinates @racket[x], @racket[y], and @racket[0], that is, on the @${XY} plane.
}

Example:
@incremental[
(vxy 2 5)
]

@defproc[(vxz [x Real] [z Real] [cs Cs (current-cs)]) Real]{
  Returns a vector in the coordinate space @racket[cs] defined by the Cartesian coordinates @racket[x], @racket[0], and @racket[z], that is, on the @${XZ} plane.
}

Example:
@incremental[
(vxz 1 8)
]

@defproc[(vyz [y Real] [z Real] [cs Cs (current-cs)]) Real]{
  Returns a vector in the coordinate space @racket[cs] defined by the Cartesian coordinates @racket[0], @racket[y], and @racket[z], that is, on the @${YZ} plane.
}

Example:
@incremental[
(vyz 3 7)
]

@defproc[(-vx [x Real 1] [cs Cs (current-cs)]) Loc]{
  Returns a vector in the coordinate space @racket[cs] defined by the Cartesian coordinates -@racket[x], @racket[0], and @racket[0], that is, on the @${X} axis.
}

Example:
@incremental[
(-vx 5)
(-vx)
]

@defproc[(-vy [y Real 1] [cs Cs (current-cs)]) Real]{
  Returns a vector in the coordinate space @racket[cs] defined by the Cartesian coordinates @racket[0], -@racket[y], and @racket[0], that is, on the @${Y} axis.
}

Example:
@incremental[
(-vy 3)
(-vy)
]

@defproc[(-vz [z Real 1] [cs Cs (current-cs)]) Real]{
  Returns a vector in the coordinate space @racket[cs] defined by the Cartesian coordinates @racket[0], @racket[0], and -@racket[z], that is, on the @${Z} axis.
}

Example:
@incremental[
(-vz 10)
(-vz)
]

@defproc[(+vxyz [v Vec] [dx Real] [dy Real] [dz Real]) Loc]{
  Returns a
  vector in the coordinate space of vector @racket[v] defined by
  adding to vector @racket[v] the translation vector defined by the
  Cartesian coordinates @racket[dx], @racket[dy], and @racket[dz].  }

Example:
@incremental[
(+vxyz (vxyz 0 0 0) 1 2 3)
(+vxyz (vxyz 1 2 3) 4 5 6)
]

@defproc[(+vx [v Vec] [dx Real]) Loc]{
  Returns a vector in the coordinate space of vector @racket[v] defined by adding to vector
  @racket[v] the translation vector defined by the Cartesian
  coordinates @racket[dx], @racket[0], and @racket[0].
}

Example:
@incremental[
(+vx (vxyz 0 0 0) 5)
]

@defproc[(+vy [v Vec] [dy Real]) Loc]{
  Returns a vector in the coordinate space of vector @racket[v] defined by adding to vector
  @racket[v] the translation vector defined by the Cartesian
  coordinates @racket[0], @racket[dy], and @racket[0].
}

Example:
@incremental[
(+vy (vxyz 1 2 3) 5)
]

@defproc[(+vz [v Vec] [dz Real]) Loc]{
  Returns a vector in the coordinate space of vector @racket[v] defined by adding to vector
  @racket[v] the translation vector defined by the Cartesian
  coordinates @racket[0], @racket[0], and @racket[dz].
}

Example:
@incremental[
(+vz (vxyz 1 2 3) 3)
]

@defproc[(+vxy [v Vec] [dx Real] [dy Real]) Loc]{
  Returns a vector in the coordinate space of vector @racket[v] defined by adding to vector
  @racket[v] the translation vector defined by the Cartesian
  coordinates @racket[dx], @racket[dy], and @racket[0].
}

Example:
@incremental[
(+vxy (vxyz 1 2 3) 1 3)
]

@defproc[(+vxz [v Vec] [dx Real] [dz Real]) Loc]{
  Returns a vector in the coordinate space of vector @racket[v] defined by adding to vector
  @racket[v] the translation vector defined by the Cartesian
  coordinates @racket[dx], @racket[0], and @racket[dz].
}

Example:
@incremental[
(+vxz (vxyz 1 2 3) 1 5)
]

@defproc[(+vyz [v Vec] [dy Real] [dz Real]) Loc]{
  Returns a vector in the coordinate space of vector @racket[v] defined by adding to vector
  @racket[v] the translation vector defined by the Cartesian
  coordinates @racket[0], @racket[dy], and @racket[dz].
}

Example:
@incremental[
(+vyz (vxyz 2 1 5) 1 2)
]

@defproc[(vpol [rho Real] [phi Real] [cs Cs (current-cs)]) Loc]{
  Returns a vector in
  the coordinate space @racket[cs] defined by the polar coordinates @racket[rho] and @racket[phi],
  corresponding to the cylindrical coordinates @racket[rho],
  @racket[phi], and @racket[0].
}

Example:
@incremental[
(vpol 1 0)
(vpol (sqrt 2) (/ pi 4))
(vpol 1 (/ pi 2))
(vpol 1 pi)
]

@defproc[(pol-rho [v Vec]) Real]{
  Returns the first polar coordinate of vector @racket[v].
}

Example:
@incremental[
(pol-rho (vpol 10 (/ pi 3)))
(pol-rho (vxyz 1 2 3))
]

@defproc[(pol-phi [v Vec]) Real]{
  Returns the second polar coordinate of vector @racket[v].
}

Example:
@incremental[
(pol-phi (vpol 10 (/ pi 3)))
(pol-phi (vxyz 1 2 3))
]

@defproc[(+vpol [v Vec] [drho Real] [dphi Real]) Loc]{
  Returns a vector in the coordinate space of vector @racket[v] defined by adding to vector
  @racket[v] the translation vector defined by the polar
  coordinates @racket[drho] and @racket[dphi].
}

Example:
@incremental[
(+vpol (vxy 1 2) (sqrt 2) (/ pi 4))
(+vpol (vxy 1 2) 1 0)
(+vpol (vxy 1 2) 1 (/ pi 2))
]

@defproc[(vcyl [rho Real] [phi Real] [z Real] [cs Cs (current-cs)]) Loc]{
  Returns a vector in
  the coordinate space @racket[cs] defined by the cylindrical coordinates @racket[rho],
  @racket[phi], and @racket[z].
}

Example:
@incremental[
(vcyl 5 0 10)
]

@defproc[(+vcyl [v Vec] [drho Real] [dphi Real] [dz Real]) Loc]{
  Returns a vector in the coordinate space of vector @racket[v] defined by adding to vector
  @racket[v] the translation vector defined by the cylindrical
  coordinates @racket[drho], @racket[dphi], and @racket[dz].
}

Example:
@incremental[
(+vcyl (vxyz 1 2 3) 3 pi/6 5)
]

@defproc[(vsph [rho Real] [phi Real] [psi Real] [cs Cs (current-cs)]) Loc]{
  Returns a vector in
  the coordinate space @racket[cs] defined by the spherical coordinates @racket[rho],
  @racket[phi], and @racket[psi].
}

Example:
@incremental[
(vsph (sqrt 3) pi/4 (atan (sqrt 2)))
]

@defproc[(+vsph [v Vec] [drho Real] [dphi Real] [dpsi Real]) Loc]{
  Returns a vector in the coordinate space of vector @racket[v] defined by adding to vector
  @racket[v] the translation vector defined by the spherical
  coordinates @racket[drho], @racket[dphi], and @racket[dpsi].
}

Example:
@incremental[
(+vsph (vxyz 1 2 3) 4 pi/4 pi/3)
]

@defproc[(u-vxyz [x Real] [y Real] [z Real] [cs Cs (current-cs)]) Vec]{
  Returns a vector in the coordinate space @racket[cs] that has
  the same direction as the vector (@racket[x], @racket[y],
  @racket[z]) but with magnitude 1.
}

Example:
@incremental[
(u-vxyz 1 2 3)
(u-vxyz 2 2 2)
]

@defproc[(unitize [v Vec]) Vec]{ 
  Returns a vector in the coordinate space of @racket[v] that has
  the same direction as the vector @racket[v] but with magnitude 1.
}

@incremental[
(unitize (vxyz 1 2 3))
(unitize (vpol 2 pi/4))
]


@section{Operations with Locations}

@defproc[(distance [p1 Loc] [p2 Loc]) Real]{
  Returns the distance between locations @racket[p1] and @racket[p2].
}

Example:
@incremental[
(distance (xyz 0 0 0) (xyz 2 4 6))
(distance (pol 2 0) (sph 5 pi/2 pi/3))
]


@section{Algebraic Operations with Locations and Vectors}

@defproc[(p-p [p Loc] [q Loc]) Vec]{
  Returns the translation vector from @racket[p] to @racket[q]. 
}

Example:
@incremental[
(p-p (xyz 1 1 1) (xyz 2 2 2))
]

@defproc[(p+v [p Loc] [v Vec]) Loc]{
  Returns the location that results
  from applying the translation described by vector @racket[v] to the
  location @racket[p].  
}

Example:
@incremental[
(p+v (xyz 1 1 1) (vxyz 2 2 2))
]

@defproc[(v+v [v1 Vec] [v2 Vec]) Vec]{
  Returns the location that results
  from applying the translation described by vector @racket[v] to the
  location @racket[p].  
}

Example:
@incremental[
(v+v (vxyz 1 1 1) (vxyz 2 2 2))
]

@defproc[(v*r [v Vec] [a Real]) Vec]{
  Returns the product between the vector @racket[v] and the scalar @racket[a].
}

Example:
@incremental[
(v*r (vxyz 1 2 3) 2)
]

@defproc[(v/r [v Vec] [a Real]) Vec]{
  Returns the division between the vector @racket[v] and the scalar @racket[a].
}

Example:
@incremental[
(v/r (vxyz 2 4 6) 2)
]

@defproc[(u0 [cs Cs (current-cs)]) Loc]{
  Returns the origin of the coordinate space @racket[cs].
}

Example:
@incremental[
(u0)
]

@section{1D Modeling}

@defproc[(point [p Loc]) Shape]{
 Creates a point at the position specified by @racket[p].
}

Example:
@incremental[
(point (xyz  1 1 5))
]

@section{2D Modeling}

@defproc[(circle [center Loc (u0)] [radius Real 1]) Shape]{ Creates a circle
  specified by its centre @racket[center] and radius @racket[radius].}

Example:
@incremental[
(circle (xyz 0 0 0) 5)
]

@defproc[(surface-circle [center Loc (u0)] [radius Real 1]) Shape]{ Creates a
  surface delimited by a circle specified by its centre @racket[center] and
  radius @racket[radius].}

Example:
@incremental[
(surface-circle (xyz 0 0 0) 5)
]

@defproc[(arc [center Loc (u0)] [radius Real 1] [start-angle Real 0]
  [amplitude Real pi]) Shape]{ Creates a circular arc specified by its
  centre @racket[center], radius @racket[radius], initial angle
  @racket[start-angle] and angle amplitude @racket[amplitude].}

Example:
@incremental[
(arc (xyz 0 0 0) 1)
(arc (xyz 0 0 0) 2 pi/4 (- pi pi/4))
]

@defproc[(surface-arc [center Loc (u0)] [radius Real 1] [start-angle Real 0]
  [amplitude Real pi]) Shape]{ Creates a surface delimited by a circular arc
  specified by its centre @racket[center], radius @racket[radius], initial
  angle @racket[start-angle] and angle amplitude @racket[amplitude].}


Example:
@incremental[
(surface-arc (xy 0 0) 10 pi/4 (* 2 pi/4))
]

@defproc[(ellipse [center Loc (u0)] [radius-x Real 1] [radius-y Real 1])
  Shape]{ Creates an ellipse specified by its centre @racket[center], major
  radius @racket[radius-x] and minor radius @racket[radius-y].}

Example:
@incremental[
(ellipse (xyz 0 0 0) 5 2)
]

@defproc[(line [pt Loc] ...) Shape]{ Creates an open polygonal line defined by
  a set of locations, specified individually or in the form of a list.}

Example:
@incremental[
(line (xy 0 0) (xy 5 0) (xy 5 5) (xy 0 5) (xy 0 0))
(line (list (xy 0 0) (xy 5 0) (xy 5 5) (xy 0 5) (xy 0 0)))
]

@defproc[(closed-line [pt Loc] ...) Shape]{ Creates a closed polygonal line
  defined by a set of locations, specified individually or in the form of a
  list.}

@defproc[(polygon [pt Loc] ...) Shape]{ Creates a closed polygonal line
  defined by a set of locations, specified individually or in the
  form of a list.}

Example:
@incremental[
(closed-line (xy 0 0) (xy 5 0) (xy 5 5) (xy 0 5))
(polygon (pol 1 (* 2 pi 0/5)) (pol 1 (* 2 pi 1/5))
         (pol 1 (* 2 pi 2/5)) (pol 1 (* 2 pi 3/5))
         (pol 1 (* 2 pi 4/5)))
]

@defproc[(surface-polygon [pt Loc] ...) Shape]{ Creates a surface delimited by
  a closed polygonal line defined by a set of locations, specified individually
  or in the form of a list.}

Example:
@incremental[
(surface-polygon (x 0) (x 1) (xy 1 1) (y 1))
]


@defproc[(spline [pt Loc] ...) Shape]{ Creates an open spline interpolating a
  set of locations. The locations can be provided individually or in the form
  of a list.  }

Example:
@incremental[
(spline (xy 0 0) (xy 5 0) (xy 5 5) (xy 0 5) (xy 0 0))
]

@defproc[(closed-spline [pt Loc] ...) Shape]{ Creates a closed spline
  interpolating a set of given locations. The locations can be provided
  individually or in the form of a list of coordinates.  }

Example:
@incremental[
(closed-spline (xy 0 0) (xy 5 0) (xy 5 5) (xy 0 5) (xy 0 0))
]

@defproc[(rectangle [p0 Loc (u0)] [p1 Loc (uxy)]) Shape]{ Creates a
  rectangle specified by the bottom-left corner @racket[p0] and
  top-right corners @racket[p1].  }

@defproc[(rectangle [c Loc (u0)] [dx Real 1] [dy Real 1]) Shape]{ Creates
  a rectangle specified by the bottom-left corners @racket[c] and the
  length @racket[dx] and width @racket[dy].  }

Example:
@incremental[
(rectangle (xy 0 0) (xy 1 1))
(rectangle (xy 0 0) 1 1)
]

@defproc[(surface-rectangle [p0 Loc (u0)] [p1 Loc]) Shape]{ Creates a
  surface delimited by a rectangle specified by the bottom-left corner
  @racket[p0] and top-right corners @racket[p1].  }

@defproc[(surface-rectangle [c Loc (u0)] [dx Real] [dy Real]) Shape]{
  Creates a surface delimited by a rectangle specified by the
  bottom-left corners @racket[c] and the length @racket[dx] and width
  @racket[dy].  }

Example:
@incremental[
(surface-rectangle (xy 0 0) (xy 10 5))
(surface-rectangle (xy 0 0) 10 5)
]

@defproc[(regular-polygon [edges Integer 3] [center Loc (u0)] [radius Real 1]
 [angle Real 0] [inscribed? Boolean #f]) Shape]{ Creates a regular polygon
 defined by the numbers of sides @racket[edges], the centre @racket[center],
 the radius @racket[radius], the initial angle with the X axis @racket[angle]
 and a Boolean value @racket[inscribed?] to specify if the polygon is inscribed
 (@racket[#t]) or circumscribed (@racket[#f]).}

Example:
@incremental[
(regular-polygon 5 (xy 0 0) 10 0 #t)
]

@defproc[(surface-regular-polygon [edges Integer 3] [center Loc (u0)] [radius Real 1]
 [angle Real 0] [inscribed? Boolean #f]) Shape]{ Creates a surface delimited by
 a regular polygon defined by the numbers of sides @racket[edges], the centre
 @racket[center], the radius @racket[radius], the initial angle with the X axis
 @racket[angle] and a Boolean value @racket[inscribed?] to specify if the
 polygon is inscribed (@racket[#t]) or circumscribed (@racket[#f]). If the
 radius value is 0 the produced result will be a point.  }

Example:
@incremental[
(surface-regular-polygon 6 (xyz 0 0 0) 1.0 0 #t)
]

@defproc[(regular-polygon-vertices [edges Integer 3] [center Loc (u0)] [radius
 Real 1] [angle Real 0] [inscribed? Boolean #f]) (Listof Loc)]{Returns a list
 containing the locations of the vertices of a regular polygon defined by the
 numbers of sides @racket[edges], the centre @racket[center], the radius
 @racket[radius], the initial angle with the X axis @racket[angle] and a
 Boolean value @racket[inscribed?] to specify if the polygon is inscribed
 (@racket[#t]) or circumscribed (@racket[#f]).}

Example:
@incremental[
(regular-polygon 5 (xy 0 0) 10 0 #t)
]


@defproc[(text [str String ""] [p Loc (u0)] [h Real 1]) Shape]{ Creates text
 from the specified string @racket[str], insertion location @racket[p] and text
 height @racket[h]. For parameterizing the text string use, e.g., the
 @racket[format] function.}

Example:
@incremental[
(text "This is a piece of text.")
(text "This is another piece of text." (xy 2 2) 5)
(text (format "How much is 1+2 ?: ~A" (+ 1 2))
        (xy 2 2) 5)
(text (format "John asked: ~A" "\"What is your name?\"")
        (xy 2 2) 5)
]

@defproc[(text-centered [str String ""] [p Loc (u0)] [h Real 1]) Shape]{
 Creates text from the specified string @racket[str], centered around the
 insertion location @racket[p] and text height @racket[h]. For parameterizing
 the text string use, e.g., the @racket[format] function.  }

Example:
@incremental[
(text-centered "This is a piece of centered text.")
(text-centered "This is another piece of centered text."
                 (xy 2 2) 5)
(text-centered (format "How much is 1+2 ?: ~A" (+ 1 2))
                 (xy 2 2) 5)
(text-centered
   (format "John asked: ~A" "\"What is your name?\"")
   (xy 2 2) 5)
]

@section{3D Modeling}

@defproc[(box [p Loc (u0)] [q Loc (xyz 1 1 1)]) Shape]{ Creates a
  box specified by two opposite corners @racket[p] and @racket[q].}

@defproc[(box [c Loc (u0)] [dx Real 1] [dy Real dx] [dz Real
  dy]) Shape]{Creates a box specified by the base insertion location @racket[c],
  length @racket[dx], width @racket[dy], and height @racket[dz].}

Example:
@incremental[
(box (xyz 0 0 0) (xyz 10 2 5))
(box (xyz 0 0 0) 10 2 5)
(box (xyz 0 0 0) 10 5)
(box (xyz 0 0 0) 10)
]

@defproc[(cone [cb Loc (u0)] [rb Real 1] [ct Loc (+z cb 1)]) Shape]{ Creates a
cone specified by the base centre @racket[cb], base radius @racket[rb], and
apex @racket[ct].}

@defproc[(cone [c Loc (u0)] [rb Real 1] [h Real 1]) Shape]{ Creates a cone
specified by the base centre @racket[cb], base radius @racket[rb], and height
@racket[h].}

Example:
@incremental[
(cone (xyz 0 0 0) 5 10)
(cone (xyz 0 0 0) 5 (xyz 1 3 10))
]

@defproc[(cone-frustum [cb Loc (u0)] [rb Real 1] [ct Loc (+z cb 1)] [rt Real
1]) Shape]{ Creates a truncated cone specified by the base centre @racket[cb],
base radius @racket[rb], apex @racket[ct], and top radius @racket[rt].}

@defproc[(cone-frustum [c Loc (u0)] [r Real 1] [h Real 1] [rt Real 1]) Shape]{
Creates a truncated cone specified by the base centre @racket[cb], base radius
@racket[rb], height @racket[h], and top radius @racket[rt].}

Example:
@incremental[
(cone-frustum (xyz 0 0 0) 5 10 3)
(cone-frustum (xyz 0 0 0) 5 (xyz 1 3 10) 3)
]

@defproc[(cylinder [cb Loc (u0)] [r Real 1] [ct Loc (+z cb 1)]) Shape]{ Creates
a cylinder specified by the base centre @racket[cb], base radius @racket[r],
and top centre @racket[ct].}

@defproc[(cylinder [c Loc (u0)] [r Real 1] [h Real 1]) Shape]{ Creates a
cylinder specified by the base centre @racket[c], base radius @racket[r], and
height @racket[h].}

Example:
@incremental[
(cylinder (xyz 0 0 0) 5 10)
(cylinder (xyz 0 0 0) 5 (xyz 1 3 10))
]

@defproc[(cuboid [b0 Loc (u0)]
                   [b1 Loc (+x b0 1)]
                   [b2 Loc (+y b1 1)]
                   [b3 Loc (+y b0 1)]
                   [t0 Loc (+z b0 1)]
                   [t1 Loc (+x t0 1)]
                   [t2 Loc (+y t1 1)]
                   [t3 Loc (+y t0 1)]) Shape]{ Creates a
cuboid with base vertices @racket[b0], @racket[b1], @racket[b2], and @racket[b3], and top vertices
@racket[t0], @racket[t1], @racket[t2], and @racket[t3]. If the cuboid faces are not planar, the result is undefined.}

Example:
@incremental[
(cuboid)
(cuboid (xyz 1 2 3) (xyz 3 5 3))
]

@defproc[(irregular-pyramid [cbs Loc (list (ux) (uy) (uxy))] [ct Loc (uz)])
Shape]{ Creates an irregular pyramid specified by the base vertices
@racket[cbs] and the top vertex @racket[ct].}

Example:
@incremental[
(irregular-pyramid)
(irregular-pyramid (list (xy 0 0) (xy 1 0) (xy 2 1) (xy 1 2)) (xyz 1 2 3))
]

@defproc[(regular-pyramid [edges Integer 4] [cb Loc (u0)] [rb Real 1] [angle Real
0] [ct Loc (+z cb 1)] [inscribed? Boolean #f]) Shape]{ Creates a regular
pyramid specified by the number of edges (or vertices) @racket[edges],
inscribed (if @racket[inscribed?]) or circuncribed (if not @racket[inscribed?])
in an imaginary cone with base centre @racket[cb], base radius @racket[rb], and
apex @racket[ct]. The pyramid has a rotation of @racket[angle] around its
axis.}

@defproc[(regular-pyramid [edges Integer 4] [cb Loc (u0)] [rb Real 1] [angle
Real 0] [h Real 1] [inscribed? Boolean #f]) Shape]{ Creates a regular pyramid
specified by the number of edges (or vertices) @racket[edges], inscribed (if
@racket[inscribed?]) or circuncribed (if not @racket[inscribed?]) in an
imaginary cone with base centre @racket[cb], base radius @racket[rb], and
height @racket[h]. The pyramid has a rotation of @racket[angle] around its
axis.}

@defproc[(regular-pyramid-frustum [edges Integer 4] [cb Loc (u0)] [rb Real 1]
[angle Real 0] [ct Loc (+z cb 1)] [rt Real 1] [inscribed? Boolean #f]) Shape]{
Creates a regular pyramid frustum specified by the number of edges (or
vertices) @racket[edges], inscribed (if @racket[inscribed?]) or circuncribed
(if not @racket[inscribed?]) in an imaginary cone frustum with base centre
@racket[cb], base radius @racket[rb], top centre @racket[ct], and top radius
@racket[rt]. The pyramid has a rotation of @racket[angle] around its axis.}

@defproc[(regular-pyramid-frustum [edges Integer 4] [cb Loc (u0)] [rb Real 1]
[angle Real 0] [h Real 1] [rt Real 1] [inscribed? Boolean #f]) Shape]{
Creates a regular pyramid frustum specified by the number of edges (or
vertices) @racket[edges], inscribed (if @racket[inscribed?]) or circuncribed
(if not @racket[inscribed?]) in an imaginary cone frustum with base centre
@racket[cb], base radius @racket[rb], height @racket[h], and top radius
@racket[rt]. The pyramid has a rotation of @racket[angle] around its axis.}

@defproc[(regular-prism [edges Integer 4] [cb Loc (u0)] [r Real 1] [angle Real
0] [ct Loc (+z cb 1)] [inscribed? Boolean #f]) Shape]{ Creates a regular prism
specified by the number of edges (or vertices) @racket[edges], inscribed (if
@racket[inscribed?]) or circuncribed (if not @racket[inscribed?]) in an
imaginary cylinder with base centre @racket[cb], radius @racket[r], and top
centre @racket[ct]. The prism has a rotation of @racket[angle] around its
axis.}

@defproc[(regular-prism [edges Integer 4] [cb Loc (u0)] [r Real 1] [angle Real
0] [h Real 1] [inscribed? Boolean #f]) Shape]{ Creates a regular pyramid
frustum specified by the number of edges (or vertices) @racket[edges],
inscribed (if @racket[inscribed?]) or circuncribed (if not @racket[inscribed?])
in an imaginary cone frustum with base centre @racket[cb], base radius
@racket[r], and height @racket[h]. The prism has a rotation of @racket[angle]
around its axis.}

@defproc[(right-cuboid [cb Loc (u0)] [width Real 1] [height Real 1] [ct Loc (+z
cb 1)]) Shape]{ Creates a right cuboid specified by the base centre
@racket[cb], width @racket[width], height @racket[height] and top centre
@racket[ct]. The cuboid will have an arbitrary rotation around its axis.}

@defproc[(right-cuboid [cb Loc (u0)] [width Real 1] [height Real 1] [h Real 1]) Shape]{
 Creates a right cuboid specified by the base centre @racket[cb],
width @racket[width], height @racket[height] and length @racket[h]. The cuboid
will have an arbitrary rotation around its axis.}

@defproc[(sphere [c Loc (u0)] [r Real 1]) Shape]{ Creates a sphere specified by
  its centre @racket[c] and radius @racket[r].}

Example:
@incremental[
(sphere (xyz 0 0 0) 10)
]


@section{Constants and Variables}

@defthing[pi Real]{ The value of @${\pi}. }
@defthing[-pi Real]{ The value of @${-\pi}. }
@defthing[2pi Real]{ The value of @${2\pi}. }
@defthing[-2pi Real]{ The value of @${-2\pi}. }
@defthing[3pi Real]{ The value of @${3\pi}. }
@defthing[-3pi Real]{ The value of @${-3\pi}. }
@defthing[4pi Real]{ The value of @${4\pi}. }
@defthing[-4pi Real]{ The value of @${-4\pi}. }
@defthing[pi/2 Real]{ The value of @${\frac{\pi}{2}}. }
@defthing[-pi/2 Real]{ The value of @${-\frac{\pi}{2}}. }
@defthing[pi/3 Real]{ The value of @${\frac{\pi}{3}}. }
@defthing[-pi/3 Real]{ The value of @${-\frac{\pi}{3}}. }
@defthing[pi/4 Real]{ The value of @${\frac{\pi}{4}}. }
@defthing[-pi/4 Real]{ The value of @${-\frac{\pi}{4}}. }
@defthing[pi/5 Real]{ The value of @${\frac{\pi}{5}}. }
@defthing[-pi/5 Real]{ The value of @${-\frac{\pi}{5}}. }
@defthing[pi/6 Real]{ The value of @${\frac{\pi}{6}}. }
@defthing[-pi/6 Real]{ The value of @${-\frac{\pi}{6}}. }
@defthing[3pi/2 Real]{ The value of @${\frac{3\pi}{2}}. }
@defthing[-3pi/2 Real]{ The value of @${-\frac{3\pi}{2}}. }

Example
@incremental[
pi
-pi
2pi
-2pi
3pi
-3pi
pi/2
-pi/2
pi/3
-pi/3
]

@section{Randomness}

@defproc[(random [k Real]) Real]{

When called with an integer argument @racket[k], returns a random
exact integer in the range @racket[0] to @math{@racket[k]-1}. When
called with zero arguments, returns a pseudo random inexact number
between @racket[0] and @racket[1], exclusive.}

Example:
@incremental[
(random 10)
(random 10)
(random 1.0)
(random 1.0)
]

@defproc[(random-range [a Real] [b Real]) Real]{

Returns a pseudo random number of the same type as @racket[a] that is
larger or equal to @racket[a] and smaller than @racket[b].}

Example:
@incremental[
(random-range 10 20)
(random-range 0.4 1.2)
]

