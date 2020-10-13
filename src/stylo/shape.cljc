(ns stylo.shape
  (:require [forge.proto :as f]
            [forge.delaunay :as delaunay]
            [stylo.svg :as svg]))

(defn parametric-grid
  [nx ny]
  (for [x (conj (mapv float (range 0 1 (/ 1 nx))) 1)
        y (conj (mapv float (range 0 1 (/ 1 ny))) 1)]
    [x y]))

(defn sample-1
  [f step]
  (let [t (range 0 1 step)]
    (map f t)))

(defn sample-2
  [f & steps]
  (for [u (range 0 1 (first steps))
        v (range 0 1 (second steps))]
    (f u v)))

(defn sample
  [f & steps]
  (let [n-params (count steps)]
    (if (= 1 n-params)
      (sample-1 f (first steps))
      (sample-2 f (first steps) (second steps)))))

#_(defn quad-path
  [u v u-step v-step]
  [[u v]
   [(+ u u-step) v]
   [(+ u u-step) (+ v v-step)]
   [u (+ v v-step)]])


;; quad-mesh only makes sense for surfaces (f u v)
#_(defn quad-mesh
  [f u-step v-step]
  (for [u (range 0 1 u-step)
        v (range 0 1 v-step)]
    (map #(apply f %) (quad-path u v u-step v-step))))

(defn subdivide-curve
  [c n]
  (let [step-size (/ 1 n)]
    (mapv #(c (float %)) (conj (into [] (range 0 1 step-size)) 1))))

(defn quad-path
  [u v u-step v-step]
  [[u v]
   [(+ u u-step) v]
   [(+ u u-step) (+ v v-step)]
   [u (+ v v-step)]])

(defn subdivide-surface
  [s [nu nv]]
  (let [u-step (/ 1 nu)
        v-step (/ 1 nv)]
    (for [u (range 0 1 u-step)
          v (range 0 1 v-step)]
      (map #(apply s %) (quad-path u v u-step v-step)))))

(def iso-euler-angles [35.264 45 0])
(def origin-angle-adjust-a [90 0 0])
(def origin-angle-adjust-b [0 90 0])

(defn rotate-points
  [pts [ax ay az]]
  (mapv #(f/rotate-point % [ax ay az]) pts))

(defn isometric-xf
  [pts]
  (-> pts
      (rotate-points origin-angle-adjust-a)
      (rotate-points origin-angle-adjust-b)
      (rotate-points iso-euler-angles)))

(defn top-xf
  [pts]
  (-> pts
      (rotate-points [0 0 0])))

(defn right-xf
  [pts]
  (-> pts
      (rotate-points [90 0 0])))

(defn drop-z
  [pts]
  (mapv #(into [] (drop-last %)) pts))

(defn axes-iso
  []
  (let [xpts (drop-z (isometric-xf [[-1000 0 0] [1000 0 0]]))
        ypts (drop-z (isometric-xf [[0 -1000 0] [0 1000 0]]))
        zpts (drop-z (isometric-xf [[0 0 -1000] [0 0 1000]]))]
    [:g#axes
     (map
      (partial svg/style-element {:stroke-width 1})
      [(svg/color "#bf616a" (apply svg/line xpts))
       (svg/color "#a3be8c" (apply svg/line ypts))
       (svg/color "#5e81ac" (apply svg/line zpts))])]))

(defn triangulate-surface
  [f grid]
  (as-> grid g
    (delaunay/triangulate g)
    (:triangles g)
    (into [] (apply concat g))
    (mapv #(apply f %) g)
    (into [] (partition 3 g))))

;; this is a bad name. 'render' implies creation of graphics... this only makes 2d points.

(defn draw-triangulation
  [triangles]
  (as-> triangles t
    (into [] (apply concat t))
    (isometric-xf t)
    (drop-z t)
    (into [] (partition 3 t))))

(def resolutions {:line 1
                  :circle 24})

(defn render-curve
  [c orientation-xf color]
  (let [res 10 ;; come up with method to select res dynamically
        pts (subdivide-curve c res)]
    (-> pts
        (orientation-xf)
        (drop-z)
        (svg/polyline)
        (#(svg/color color %)))))

(defn render-curves 
  [shape xf color]
  (map #(render-curve % xf color) (:curves shape)))

(defn render-subsurface
  [s orientation-xf style]
  (-> s
      (orientation-xf)
      (drop-z)
      (svg/polygon)
      (#(svg/style-element style %))))

(defn render-surfaces
  [shape xf style]
  (let [gons (mapcat #(subdivide-surface % [11 3]) (:surfaces shape))
        gons2 (reverse (sort-by (comp (partial f/distance [1000 1000 1000]) f/centroid) gons))]
    (map #(render-subsurface % xf style) gons2)))
