# Standing Point
This is a simple design for a small standing desk. You can build it all out of plywood.

## Materials
The entire desk can be built from 0.5in plywood, and can be either glued or bolted together. If you bolt the desk together, you can disassemble it for easier storage and moving later.

## Major Dimensions
The table top is 32in wide by 20in deep. The surface stands 39.5in up from the ground.

{ex}
(ns standing-point
  (:require [hiccup.core :refer [h html]]
            [stylo.draw :refer :all]))

(load-file "chunk.clj")
(use 'stylo.chunk)

(def gt 1)
(def gw 24)
(def gd 16)
(def gh 39.5)
(def glw 2.5)
{ex}

# Parts
The desk consists of only a few unique parts:
- top
- shelf
- legs

## Top
The top is quite simple. It has 4 slots that snugly 'trap' the legs in place. This allows the whole desk to be held together by only a few bolts holding the top to the shelf components.

{ex}
(def top-sk (polygon-2d
    [[0 0]
     [gw 0]
     [gw gd]
     [0 gd]
     #_[]]))

(def top-cut-sk (polygon-2d
    [[0 0]
     [glw 0]
     [glw gt]
     [0 gt]]))

(def top-base
  (extrude- top-sk gt))

(def top-cut
  (extrude- top-cut-sk gt))
  
(def top
  (let [o 1]
    (join 
      top-base
      (mv- top-cut [o o 0])
      (mv- top-cut [(- gw o glw) o 0])
      (mv- top-cut [(- gw o glw) (- gd gt gt) 0])
      (mv- top-cut [o (- gd gt gt) 0]))))

(figure [720 325 (/ 25.4 2)]
  "Desk Top Design"
  (mv [0 5] 
    (draw-edges-recursive top)
    (h-dimension [0 0] [gw 0] (str gw "in"))
    (v-dimension [gw 0] [gw gd] (str gd "in"))))

(cad-view [600 500 8] "Desk Top CAD View" top)

{ex}

## Shelf
The shelf is the most substantial portion of the desk. It uses a tab/slot construction to hold the legs and horizontal pieces together.

## Legs
The table legs will be cut from 0.5in plywood.

{ex}
(def leg-sk (polygon-2d
  (let [legw 3
        legl (- gh gt)
        cuth (/ legl 4)]
    [[0 0]
     [cuth 0]
     [cuth gt]
     [(* 2 cuth) gt]
     [(* 2 cuth) 0]
     [(* 3 cuth) 0]
     [(* 3 cuth) gt]
     [legl gt]
     [legl legw]
     [0 legw]])))

(def leg-a
  (extrude- leg-sk gt))
(def leg-b
  (rot- 
    (mv- (rot- leg-a [90 0 180]) [(- gh gt) 0 0])
    [180 0 0]))

(def leg 
  (map #(mv- % [2 0 0]) [leg-a leg-b]))

(figure [720 280 (/ 25.4 2)]
  "leg-side-a design"
  (mv [10 5] (draw-edges-recursive leg)))

{ex}

## The shelf
The shelf design.

## CAD View WIP
This is a CAD view:

{ex}
(def cylinder
     (rot- (extrude- (circle 1) 6) [90 90 0]))

(def hexagon
     (rot- (extrude- (regular-polygon 2.5 6) 1.5) [90 90 0]))

(def example-bolt (join
                  hexagon
                  (mv- cylinder [2 0 0])))

(cad-view [500 300 10] "example bolt." example-bolt)
{ex}

