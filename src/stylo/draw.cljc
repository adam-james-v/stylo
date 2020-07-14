(ns stylo.draw)

(defn svg
  [[w h sc] & content]
  [:svg {:width w
         :height h
         :viewbox (str "-1 -1 " w " " h)
         :xmlns "http://www.w3.org/2000/svg"}
   [:g {:transform (str "scale(" sc ")")} content]])

(defn figure
  ([descr content]
   (figure [500 250 25] descr content))

  ([[w h sc] descr & content]
   [:div.figure
    (svg [w h sc] content)
    [:p descr]]))

(defn quadrant-figure
  ([descr q1 q2 q3 q4]
   (quadrant-figure [720 720 25] descr q1 q2 q3 q4))
  
  ([[w h sc] descr q1 q2 q3 q4]
   (let [qw (/ w 2.0)
         qh (/ h 2.0)]
     [:div.figure 
      [:div 
       {:style {:display "flex"
                :flex-wrap "wrap"
                :width (str w "px")}}
       (svg [qw qh sc] q2)
       (svg [qw qh sc] q1)
       (svg [qw qh sc] q3)
       (svg [qw qh sc] q4)]
      [:p descr]])))

(defn grid-figure
  "creates a figure with an evenly-spaced WxH grid of svg elements."
  []
  "NOT IMPLEMENTED YET")

(defn pt-str
  [pts]
  (apply str (map #(apply str (interleave % ["," " "])) pts)))

(defn path-str
  [pts]
  (let [line-to #(str "L" (first %) " " (second %))
        move-to #(str "M" (first %) " " (second %))]
    (str (move-to (first pts)) " "
         (apply str (interleave 
                     (map line-to (rest pts))
                     (repeat " ")))
         "Z")))

(defn scale-str
  [sc]
  (str "scale(" sc ")"))

(defn translate-str
  [x y]
  (str "translate(" x " " y ")"))

(defn rotate-str
  ([r]
   (str "rotate(" r ")"))
  ([r [x y]]
   (str "rotate(" r " " x " " y ")")))

(defn hsl-str
  [h s l]
  (str "hsl(" h ", " s "%, " l "%)"))

(defn bb-center
  [pts]
  (let [xs (map first pts)
        ys (map last pts)
        xmax (apply max xs)
        ymax (apply max ys)
        xmin (apply min xs)
        ymin (apply min ys)]
    [(+ (/ (- xmax xmin) 2.0) xmin)
     (+ (/ (- ymax ymin) 2.0) ymin)]))

(defn distance
  "compute distance between two points"
  [a b]
  (let [v (map - b a)
        v2 (apply + (map * v v))]
    (Math/sqrt v2)))

(defn -line
  [a b]
  (fn [t]
    (cond
      (= (float t) 0.0) a
      (= (float t) 1.0) b
      :else
      (mapv + a (map * (map - b a) (repeat t))))))

(defn slope
  [f]
  (let [[x1 y1] (f 0)
        [x2 y2] (f 1)]
    (/ (- y2 y1) (- x2 x1))))

(defn parallel?
  [la lb]
  (= (slope la) (slope lb)))

(defn angle-between-lines
  [la lb]
  (if-not (parallel? la lb)  
    (let [m1 (slope la)
          m2 (slope lb)]
      (Math/atan (/ (- m1 m2) 
                    (+ 1 (* m1 m2)))))
    0))

(defn d->t
  [f d]
  (let [l (distance (f 0) (f 1))]
    (/ d l)))

(defn t->d
  [f t]
  (distance (f 0) (f t)))

(defn perpendicular
  [[x y]]
  [(- y) x])

(defn normalize
  "find the unit vector of a given vector"
  [v]
  (let [m (Math/sqrt (reduce + (map * v v)))]
    (mapv / v (repeat m))))

(defn det
  [a b]
  (- (* (first a) (second b)) 
     (* (second a) (first b))))

;; this should be improved
;; currently can cause divide by zero issues

(defn line-intersection
  [[a b] [c d]]
  (let [[ax ay] a
        [bx by] b
        [cx cy] c
        [dx dy] d
        xdiff [(- ax bx) (- cx dx)]
        ydiff [(- ay by) (- cy dy)]
        div (det xdiff ydiff)
        d [(det a b) (det c d)]
        x (/ (det d xdiff) div)
        y (/ (det d ydiff) div)]
    [x y]))

(defn offset-edge
  [[a b] d]
  (let [p (perpendicular (mapv - b a))
        pd (map * (normalize p) (repeat (- d)))
        xa (mapv + a pd)
        xb (mapv + b pd)]
    [xa xb]))

(defn cycle-pairs
  [pts]
  (let [n (count pts)]
    (vec (take n (partition 2 1 (cycle pts))))))

(defn every-other
  [v]
  (let [n (count v)]
    (map #(get v %) (filter even? (range n)))))

(defn offset
  [pts d]
  (let [edges (cycle-pairs pts)
        opts (mapcat #(offset-edge % d) edges)
        oedges (every-other (cycle-pairs opts))
        edge-pairs (cycle-pairs oedges)]
    (map #(apply line-intersection %) edge-pairs)))

(defn sc
  [sc & elems]
  (into [:g {:transform (scale-str sc)}] elems))

(defn mv
  [[x y] & elems]
  (into [:g {:transform (translate-str x y)}] elems))

(defn rot
  [r [x y] & elems]
  (into [:g {:transform (rotate-str r [x y])}] elems))

(defn rect
  ([w h]
   (rect w h nil))
  ([w h col]
   [:rect {:fill (if col col "black")
           :stroke (if col col "black")
           :stroke-width 2
           :width w
           :height h}]))

(defn polygon
  ([pts]
   (polygon pts nil))
  ([pts col]
   [:polygon {:class ["ln" (if col col "clr")]
              :points (pt-str pts)}]))

(defn polygon-d
  ([pts]
   (polygon-d pts nil))
  ([pts col]
   [:polygon {:class ["ln-d" (if col col "clr")]
              :points (pt-str pts)}]))

(defn closed-path
  ([pts]
   (closed-path pts nil))
  ([pts col]
   [:path {:class ["ln" (if col col "clr")]
           :d (path-str pts)}]))

(defn poly-path
  ([paths]
   (poly-path paths nil))
  ([paths col]
   (let [path-strs (map path-str paths)]
     [:path {:class ["ln" (if col col "clr")]
             :d (apply str (interleave path-strs (repeat " ")))}])))

(defn label
  [text]
  [:text {:fill "black"
          :x -4
          :y 4
          :font-family "Verdana"
          :font-size 12
          :transform "translate(0,0) scale(0.05)"} text])

(defn ln
  [a b]
  [:polyline {:stroke-linecap "round"
              :stroke "black"
              :stroke-width "2"
              :fill "rgba(0,0,0,0)"
              :points (pt-str [a b])}])

(defn ln-d
  [a b]
  [:polyline {:stroke-linecap "round"
              :stroke-dasharray "4, 5"
              :stroke "black"
              :stroke-width "1.5"
              :fill "rgba(0,0,0,0)"
              :points (pt-str [a b])}])

(defn arw
  [a b]
  [:g {}
   [:marker {:id "head"
             :orient "auto"
             :markerWidth "0.5"
             :markerHeight "1"
             :refX "0.025"
             :refY "0.25"}
    [:path {:d "M0,0 V0.5 L0.25,0.25 Z"
            :fill "black"}]]
   [:polyline {:marker-end "url(#head)"
               :stroke "black"
               :stroke-width "2"
               :fill "rgba(0,0,0,0)"
               :points (pt-str [a b])}]])

(defn h-dimension
  [a b text]
  (let [a (map - a [0 2])
        b (map - b [0 2])
        mid (bb-center [a b])
        label-offset (* 0.225 (count text))]
    (list 
     (mv (map - mid [label-offset 0]) (sc 2 (label text)))
     (ln (map - a [0 0.75]) (map + a [0 1.5]))
     (ln (map - b [0 0.75]) (map + b [0 1.5]))
     (arw (map - mid [(* 1.75 label-offset) 0])  
          (map + a [0.5 0]))
     (arw (map + mid [(* 1.75 label-offset) 0]) 
          (map - b [0.5 0])))))

(defn v-dimension
  [a b text]
  (let [a (map + a [2 0])
        b (map + b [2 0])
        mid (bb-center [a b])
        label-offset (* 0.225 (count text))]
    (list 
     (mv (map - mid [label-offset 0]) (rot 90 [label-offset 0] (sc 2(label text))))
     (ln (map - a [1.5 0]) (map + a [0.75 0]))
     (ln (map - b [1.5 0]) (map + b [0.75 0]))
     (arw (map - mid [0 (* 1.75 label-offset)])  
          (map + a [0 0.5]))
     (arw (map + mid [0 (* 1.75 label-offset)]) 
          (map - b [0 0.5])))))

(defn dimension
  [a b]
  (let [text (format "%.2f" (distance a b))
        label-offset (* 0.225 (count text))
        label-angle (Math/toDegrees (angle-between-lines (-line a b) (-line [0 0] [1 0])))
        [ao bo] (offset-edge [a b] 2)
        mid (bb-center [ao bo])
        arw-a (-line mid ao)
        arw-b (-line mid bo)
        arw-t (- 1 (d->t arw-a 0.5))
        mid-t (d->t arw-a (* 1.75 label-offset))
        la (-line a ao)
        lb (-line b bo)
        [lat1 lat2] (map (partial d->t la) [0.5 2.5])
        [lbt1 lbt2] (map (partial d->t lb) [0.5 2.5])]
    (list
     (arw (arw-a mid-t) (arw-a arw-t))
     (arw (arw-b mid-t) (arw-b arw-t))
     (ln (la lat1) (la lat2))
     (ln (lb lbt1) (lb lbt2))
     (mv (map - mid [label-offset 0]) (rot label-angle [label-offset 0] (sc 2 (label text)))))))

(defn dot
  [[x y]]
  [:circle {:class ["attn"]
            :cx x :cy y :r 0.125}])
