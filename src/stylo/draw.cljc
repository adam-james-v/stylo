(ns stylo.draw
  (:require [stylo.geom :as g]
            [stylo.svg :as svg]
            [stylo.delaunay :as delaunay]))

(defn figure
  [[w h sc fig-num] descr & content]
  [:div.figure
   (svg/svg [w h sc] content)
   [:p 
    (when fig-num [:strong (str "Fig. " fig-num " ")]) 
    descr]])

(defn fig
  [fig-num descr & content]
  [:div.figure
   content
   [:p [:strong (str "Fig. " fig-num " ")] descr]])

(defn dwg-2d
  [[w h sc] & content]
  (let [view-str (apply str (interpose " " 
                                       [(/ w -2.0) (/ h -2.0) w h]))]
    (assoc-in (svg/svg [w h sc] content) [1 :viewBox] view-str)))

(defn axes-2d
  []
  [:g#axes
   (map
    (partial svg/style-element {:stroke-width 1})
    [(svg/color "#a3be8c" (svg/line [0 -1000] [0 1000]))
     (svg/color "#bf616a" (svg/line [-1000 0] [1000 0]))])])

(defn drop-z
  [pts]
  (mapv #(into [] (drop-last %)) pts))

(defn add-z
  [pts]
  (mapv #(conj % 0) pts))

(defn sin-cos-pair [theta]
  [(Math/sin (g/to-rad theta)) (Math/cos (g/to-rad theta))])

(defn -rot-pt
  [[x y] theta]
  (let [[s-t c-t] (sin-cos-pair theta)]
    [(- (* x c-t) (* y s-t))
     (+ (* y c-t) (* x s-t))]))

;; this rotates a point around [0,0,0]
(defn rot-pt
  [axis theta [x y z]]
  (cond
    (= axis :x) (into [x] (-rot-pt [y z] theta))
    (= axis :y) (apply #(into [] [%2 y %1]) (-rot-pt [z x] theta))
    (= axis :z) (into (-rot-pt [x y] theta) [z])))

(defn -rot
  [pts axis theta]
  (let [xpts (mapv (partial rot-pt axis theta) pts)]
    xpts))

(defn rot-
  [pts [th-x th-y th-z]]
  (-> pts
      (-rot :z th-z)
      (-rot :y th-y)
      (-rot :x th-x)))
;; euler angles adjusted according to wikipedia 'isometric projection' page
;; precise value for the first rotation is arcsin(1/sqrt(3))

(def iso-euler-angles [35.264 45 0])
(def origin-angle-adjust-a [90 0 0])
(def origin-angle-adjust-b [0 90 0])
(defn orient-iso
  [pts]
  (-> pts
      (rot- origin-angle-adjust-a)
      (rot- origin-angle-adjust-b)
      (rot- iso-euler-angles)))

(defn orient-xy
  [pts]
  (-> pts
      (rot- [0 0 0])))

(defn axes-iso
  []
  (let [xpts (drop-z (orient-iso [[-1000 0 0] [1000 0 0]]))
        ypts (drop-z (orient-iso [[0 -1000 0] [0 1000 0]]))
        zpts (drop-z (orient-iso [[0 0 -1000] [0 0 1000]]))]
    [:g#axes
     (map
      (partial svg/style-element {:stroke-width 1})
      [(svg/color "#bf616a" (apply svg/line xpts))
       (svg/color "#a3be8c" (apply svg/line ypts))
       (svg/color "#5e81ac" (apply svg/line zpts))])]))



#_(defn render-triangulation
  [triangles]
  (as-> triangles t
    (into [] (apply concat t))
    (orient-iso t)
    (drop-z t)
    (into [] (partition 3 t))))

(defn transform-points
  [pts]
  (-> pts 
      (orient-iso)
      (drop-z)))

(defn render-3d
  [cxf & content]
  "WIP")

(defn svg3d
  [[w h sc cxf] & content]
  (let [content (render-3d cxf content)]
    (svg/svg [w h sc] content)))

(defn circle
  [r]
  [:circle {:cx 0 :cy 0 :cz 0 :r r}])

(defn ellipse
  [rx ry]
  [:ellipse {:cx 0 :cy 0 :cz 0 :rx rx :ry ry}])

(defn line
  [[ax ay az] [bx by bz]]
  [:line {:x1 ax :y1 ay :z1 az :x2 bx :y2 by :z2 bz}])

(defn path
  [d]
  [:path {:d d
          :fill-rule "evenodd"}])

(defn polygon
  [pts]
  [:polygon {:points (svg/points->str pts)}])

(defn polyline
  [pts]
  [:polyline {:points (svg/points->str pts)}])

(defn rect
  [w h]
  [:rect {:width w :height h :x (/ w -2.0) :y (/ h -2.0)}])

(defn line3d->line2d
  [orientation-fn line]
  (let [pts [(map #(get (last line) %) [:x1 :y1 :z1])
             (map #(get (last line) %) [:x2 :y2 :z2])]
        xf-pts (orientation-fn pts)]
    (apply svg/line xf-pts)))

(defmulti svg3d->svg
  (fn [_ element]
    (first element)))

;; cxf is camera-transform-fn (eg. orient-iso)

(defmethod svg3d->svg :line
  [cxf [k props]]
  (let [pts [(map #(get props %) [:x1 :y1 :z1])
             (map #(get props %) [:x2 :y2 :z2])]
        xf-pts (cxf pts)]
    (apply svg/line xf-pts)))

(defn parametric-grid
  [nx ny]
  (for [x (conj (mapv float (range 0 1 (/ 1 nx))) 1)
        y (conj (mapv float (range 0 1 (/ 1 ny))) 1)]
    [x y]))

(defn brep-sphere
  [r]
  (fn [u v]
    (let [[u v] (map #(* 2 Math/PI %) [u v])
          x (* r (Math/sin u) (Math/cos v))
          y (* r (Math/sin u) (Math/sin v))
          z (* r (Math/cos u))]
      [x y z])))

(defn brep-circle
  [r]
  (fn [t]
    (let [t (* 2 Math/PI t)
          x (* r (Math/cos t))
          y (* r (Math/sin t))]
      [x y 0])))

(defn brep-cylinder
  [r h]
  (fn [u v]
    (let [u (* 2 Math/PI u)
          v (* h v)
          x (* r (Math/cos u))
          y (* r (Math/sin u))
          z v]
      [x y z])))

(defn brep-torus
  [R r]
  (fn [u v]
    (let [[u v] (map #(* 2 Math/PI %) [u v])
          x (* (+ R (* r (Math/cos u))) (Math/cos v))
          y (* (+ R (* r (Math/cos u))) (Math/sin v))
          z (* r (Math/sin u))]
      [x y z])))

(defn triangulate-surface
  [f grid]
  (as-> grid g
    (delaunay/triangulate g)
    (:triangles g)
    (into [] (apply concat g))
    (mapv #(apply f %) g)
    (into [] (partition 3 g))))

;; this is a bad name. 'render' implies creation of graphics... this only makes 2d points.

(defn render-triangulation
  [triangles]
  (as-> triangles t
    (into [] (apply concat t))
    (orient-iso t)
    (drop-z t)
    (into [] (partition 3 t))))

(defn scad-pt-str
  [pt]
  (str "["
       (apply str (interpose ", " pt))
       "]"))

(defn scad-polyhedron
  [triangles]
  (let [points (apply concat triangles)
        faces (partition 3 (range (count points)))]
    (str
     "polyhedron( points = [ "
     (apply str (interpose ", " (map scad-pt-str points)))
     "],\n"
     "faces = [ "
     (apply str (interpose ", " (map scad-pt-str faces)))
     "], convexity = 10);")))
