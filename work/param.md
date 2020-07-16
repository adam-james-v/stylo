# Parametric WIP

This is just a testing document for parametric ideas.
My approach to 2D/3D drawing in stylo is messy. This is a brief exploration into Functional Rep, Boundary Rep, signed distance functions, parametric equations, and how to wire them all together in a useful way.

What kind of model and data structure actually makes sense for drawing 2D and 3D diagrams?

(ex

(ns param
  (:require [hiccup.core :refer [h html]]
            [stylo.draw :refer :all]))

(load-file "param.clj")
(use 'stylo.parametric)

(def xsphere
  (-> (brep-sphere 8)
      (brep-rotate isometric-euler-angles)
      (brep-translate [10 10 0])))

(def dot-sphere (render-dot xsphere))
(figure [500 200 10] "dot-rendering" dot-sphere)

(def slice-sphere (render-slice xsphere))
(figure [500 200 10] "slice-rendering" slice-sphere)

(def quad-sphere (render-quad xsphere))
(figure [500 200 10] "quad-rendering" quad-sphere)

(def xrect
  (-> (brep-axis-aligned-rect 12 20 [0 1 0])
      (brep-rotate isometric-euler-angles)
      (brep-translate [10 10 0])))

(figure [500 200 10] "brep-rect-wip"
        (render-dot xrect))

(def xcirc
  (-> (brep-circle 8)
      (brep-rotate isometric-euler-angles)
      (brep-translate [10 10 0])))

(figure [500 200 10] "brep-rect-wip"
        (render-dot-curve xcirc))

ex)

.
