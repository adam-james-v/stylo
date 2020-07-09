(ex
(load-file "fabric.clj")
(use 'qpunk.fabric)
(html (list

[:svg {:style "width: 0; height: 0; position: absolute;"}
  [:pattern {:id "BvHatch" :patternUnits "userSpaceOnUse" :width 0.2 :height 0.5}
    [:rect {:width 0.1 :height 0.5 :style "fill:rgb(211, 126, 120);"}]
    [:g {:transform "translate(0.1,0)"} [:rect {:width 0.1 :height 0.5 :style "fill:rgb(235, 171, 164);"}]]]
  [:pattern {:id "BhHatch" :patternUnits "userSpaceOnUse" :width 0.5 :height 0.2}
    [:rect {:width 0.5 :height 0.1 :style "fill:rgb(211, 126, 120);"}]
    [:g {:transform "translate(0,0.1)"} [:rect {:width 0.5 :height 0.1 :style "fill:rgb(235, 171, 164);"}]]]

  [:pattern {:id "FvHatch" :patternUnits "userSpaceOnUse" :width 0.2 :height 0.5}
    [:rect {:width 0.1 :height 0.5 :style "fill:rgb(111, 131, 156);"}]
    [:g {:transform "translate(0.1,0)"} [:rect {:width 0.1 :height 0.5 :style "fill:rgb(120, 168, 227);"}]]]
  [:pattern {:id "FhHatch" :patternUnits "userSpaceOnUse" :width 0.5 :height 0.2}
    [:rect {:width 0.5 :height 0.1 :style "fill:rgb(111, 131, 156);"}]
    [:g {:transform "translate(0,0.1)"} [:rect {:width 0.5 :height 0.1 :style "fill:rgb(120, 168, 227);"}]]]]

fabric-styles
[:style "
table{width:auto;}
th, td{vertical-align:top;padding:5px;border: 1px solid #ddd;}
table ul {list-style-type:none;padding-left:4px;margin:0}
table p {margin:0}
table li:before {content:'▢ ';}
@media print {.pagebreak {page-break-before:always;}}
.figure{padding-left:7px;}
.figure p{font-size:smaller;font-style:italic;}
.trim{fill:#fff;stroke:#fff !important;}
.A{fill:rgb(235, 235, 220);}
.Aw{fill:rgb(242, 242, 242);}
.B{fill:rgb(178, 113, 106);}
.Bv{fill:url(#BvHatch);}
.Bh{fill:url(#BhHatch);}
.C{fill:rgb(65, 95, 135);}
.Cw{fill:rgba(140, 154, 168);}
.Cw-a{fill:rgba(140, 154, 168, 0.5);}
.D{fill:rgb(240, 185, 158);}
.E{fill:rgb(158, 189, 221);}
.F{fill:rgb(111, 131, 156);}
.Fv{fill:url(#FvHatch);}
.Fh{fill:url(#FhHatch);}
"]))
ex)

# Example begins

(ex
(html [:h2 [:img {:style "width:50px;padding-right:10px;display:inline;" :src "./resources/berry.png"}] "The Blueberry Method"])
ex)

1. Draw a diagonal line on the wrong side of 4 squares designated for the Blueberry.
2. Place a Blueberry square RST onto the corners that meet in the middle of 4 selected units. Sew on the diagonal lines. Press two towards the corner, and then press the opposite two away from the corner. This will help to nest the seams together when completing The Blueberry. Trim.
3. Sew the seams together and your Blueberry will be complete.

(ex
(def bb-a 
  (list
   (sq 1.25 "Cw")
   (ln-d [-0.1 1.35] [1.35 -0.1])))

(defn bb-b
  [fabric]
  (list
   (sq 2.5 fabric)
   (mv [1.25 1.25] bb-a)))

(defn bb-c
  [fabric]
  (list
   (sq 2.5 fabric)
   (mv [1.25 1.25] bb-a)
   (mv [1.4 1.4] (rot 180 [0.625 0.625] (hst 1.25 "trim"))
                 (mv [0.075 0.075] (rot 180 [0.55 0.55] (hst 0.95 "Cw-a"))))))

(defn bb-d
  [fabric out?]
  (list
   (sq 2.5 fabric)
   (mv [1.25 1.25] (rot 180 [0.625 0.625] (hst 1.25 "C"))
                   (if out?
                    [:g {:transform "scale(0.625)"} (arw [-0.1 -0.1] [1.35 1.35])]
                    [:g {:transform "scale(0.625)"} (arw [1.35 1.35] [-0.1 -0.1])]))))

(defn bb-e
  [fabric]
  (list
   (sq 2 fabric)
   (mv [1.25 1.25] (rot 180 [0.375 0.375] (hst 0.75 "C")))))

(html [:div {:class "figure"}
  (svg [700 160 22.5]
    (mv [0.1 1.25]
      bb-a
      (mv [1.5    0] (rot  90 [0.625 0.625] bb-a))
      (mv [1.75 1.75] (rot 180 [0.625 0.625] bb-a))
      (mv [   0 1.75] (rot 270 [0.625 0.625] bb-a)))
    (mv [0.5 5.375] (label "mark on"))
    (mv [0.175 6.25] (label "wrong side"))
    
    (mv [5 0]
      (bb-b "A")
      (mv [3 0] (rot  90 [1.25 1.25] (bb-b "Bv")))
      (mv [3 3] (rot 180 [1.25 1.25] (bb-b "A")))
      (mv [0 3] (rot 270 [1.25 1.25] (bb-b "Bh"))))
    (mv [5.825 6.25] (label "sew diagonals"))
    
    (mv [12.25 0]
      (bb-c "A")
      (mv [3 0] (rot  90 [1.25 1.25] (bb-c "Bh")))
      (mv [3 3] (rot 180 [1.25 1.25] (bb-c "A")))
      (mv [0 3] (rot 270 [1.25 1.25] (bb-c "Bh"))))
    (mv [14.625 6.25] (label "trim"))
    
    (mv [19.5 0]
      (bb-d "A" false)
      (mv [3 0] (rot  90 [1.25 1.25] (bb-d "Bh" true)))
      (mv [3 3] (rot 180 [1.25 1.25] (bb-d "A" false)))
      (mv [0 3] (rot 270 [1.25 1.25] (bb-d "Bh" true))))
    (mv [21.625 6.25] (label "press"))
    
    (mv [26.75 0.5]
      (bb-e "A")
      (mv [2 0] (rot  90 [1 1] (bb-e "Bh")))
      (mv [2 2] (rot 180 [1 1] (bb-e "A")))
      (mv [0 2] (rot 270 [1 1] (bb-e "Bh"))))
    (mv [27.5 6.25] (label "sew seams")))


  [:p "Blueberry Point Method"]])
ex)

That's the end of the sample.

