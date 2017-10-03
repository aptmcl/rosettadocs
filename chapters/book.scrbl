#lang scribble/manual
@(require racket/date)
@(require "../rosetta.rkt")
@setup-math


@title[#:date (date->string (current-date))]{Programming for Architecture}
@author{António Menezes Leitão}

@table-of-contents[]

@include-section["preface.scrbl"]
@include-section["programming.scrbl"]
@include-section["modeling.scrbl"]
@include-section["recursion.scrbl"]
@include-section["state.scrbl"]
@include-section["structures.scrbl"]
@include-section["shapes.scrbl"]
@include-section["transformations.scrbl"]
@include-section["higher-order.scrbl"]
@include-section["parametrics.scrbl"]
@include-section["epilogue.scrbl"]
@include-section["operators.scrbl"]
@index-section[]