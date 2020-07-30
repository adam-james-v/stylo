(ns stylo.draw
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [clojure.spec.gen.alpha :as gen]
            [clojure.test :as test]))

(s/def ::pt2d (s/tuple number? number?))
(s/def ::pt3d (s/tuple number? number? number?))
(s/def ::pt (s/or :xy ::pt2d :xyz ::pt3d))
(s/def ::pts (s/* ::pt))
(s/def ::axis #{:x :y :z})

(s/def ::parameter (s/and number? #(<= 0 % 1)))
(s/def ::surface-parameter (s/tuple ::parameter ::parameter))
(s/def ::quad (s/tuple number? number? number? number?))
(s/def ::path (s/* ::pt))


;; potential issue: ::quad is indistinguishable from ::pt2d because they are both just tuples with numbers

(defn svg
  [[w h sc] & content]
  [:svg {:width w
         :height h
         :ViewBox (str "-1 -1 " w " " h)
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

(defn point-to-string
  [pt]
  (apply str (interpose "," pt)))

(defn pt-str
  [pts]
  (apply str (interpose " " (map point-to-string pts))))

(defn path-str
  [pts]
  (let [line-to #(str "L" (first %) " " (second %))
        move-to #(str "M" (first %) " " (second %))]
    (str 
     (move-to (first pts)) " "
     (apply str (interpose " " (map line-to (rest pts))))
     " Z")))

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

(defn round
  [num places]
  (let [d (Math/pow 10 places)]
    (/ (Math/round (* num d)) d)))

(defn to-deg
  [rad]
  (* rad (/ 180 Math/PI)))

(defn to-rad
  [deg]
  (* deg (/ Math/PI 180)))

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
  "Computes the distance between two points."
  [a b]
  (let [v (map - b a)
        v2 (apply + (map * v v))]
    (Math/sqrt v2)))

(defn *c3d
  "calculates cross product of two 3d-vectors"
  [a b]
  (let [[a1 a2 a3] a
        [b1 b2 b3] b
        i (- (* a2 b3) (* a3 b2))
        j (- (* a3 b1) (* a1 b3))
        k (- (* a1 b2) (* a2 b1))]
    [i j k]))

;; this fn will tell you the parameter that correspondss to the distance along the line
(defn d->t
  [f d]
  (let [l (distance (f 0) (f 1))]
    (/ d l)))

;; fn will tell you the distance along the line that parameter's point is.
(defn t->d
  [f t]
  (distance (f 0) (f t)))

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

;; slice only makes sense with 2+ dims? 
(defn slice
  [f u-step v-step]
  (for [u (range 0 1 u-step)]
    (for [v (range 0 1 v-step)]
      (f u v))))

(defn quad-path
  [u v u-step v-step]
  [[u v]
   [(+ u u-step) v]
   [(+ u u-step) (+ v v-step)]
   [u (+ v v-step)]])

;; quad-mesh only makes sense for surfaces (f u v)
(defn quad-mesh
  [f u-step v-step]
  (for [u (range 0 1 u-step)
        v (range 0 1 v-step)]
    (map #(apply f %) (quad-path u v u-step v-step))))

(defn translate
  [pts [mx my mz]]
  (map #(map + % [mx my mz]) pts))

(defn brep-translate
  [f [mx my mz]]
  (fn [& params]
    (mapv + (apply f params) [mx my mz])))

(defn sin-cos [theta]
  "give the results of sin and cos of theta(degrees) as [s c]"
  [(Math/sin (Math/toRadians theta)) 
   (Math/cos (Math/toRadians theta))])

(defn- rotate-pt-helper
  [[a b] theta]
  (let [[s-t c-t] (sin-cos theta)]
    [(- (* a c-t) (* b s-t))
     (+ (* b c-t) (* a s-t))]))

(defn rotate-pt
  [pt axis theta]
  (let [[x y z] pt]
    (cond
      (= axis :x) (into [x] (rotate-pt-helper [y z] theta))
      (= axis :y) (apply #(into [] [%2 y %1]) (rotate-pt-helper [z x] theta))
      (= axis :z) (into (rotate-pt-helper [x y] theta) [z]))))

(s/fdef rotate-pt
  :args (s/cat :pt ::pt3d :axis ::axis :theta number?)
  :ret  ::pt)

(defn rotate-about-axis
  [pts axis theta]
  (map #(rotate-pt % axis theta) pts))

(defn rotate-euler
  [pts [th-x th-y th-z]]
  (-> pts
    (rotate-about-axis :z th-z)
    (rotate-about-axis :y th-y)
    (rotate-about-axis :x th-x)))

(def isometric-euler-angles [30 45 0])

(defn brep-rotate
  [f [th-x th-y th-z]]
  (fn [& params]
    (-> (apply f params)
        (rotate-pt :z th-z)
        (rotate-pt :y th-y)
        (rotate-pt :x th-x))))

(defn -circle
  [r]
  (fn [t]
    (let [t (* 2 Math/PI t)
          x (* r (Math/cos t))
          y (* r (Math/sin t))]
      [x y 0])))

(defn -ellipse
  [rx ry]
  (fn [t]
    (let [t (* 2 Math/PI t)
          x (* rx (Math/cos t))
          y (* ry (Math/sin t))]
      [x y])))

;; Functional Representation
;; SDF signed distance functions

(defn frep-sphere [r]
  (fn [[x y z]]
    (+ (* x x) (* y y) (* z z) (- (* r r)))))

;; Boundary Representation
(defn brep-sphere
  [r]
  (fn [u v]
    (let [[u v] (map #(* 2 Math/PI %) [u v])
          x (* r (Math/sin u) (Math/cos v))
          y (* r (Math/sin u) (Math/sin v))
          z (* r (Math/cos u))]
      [x y z])))

(defn sphere-idea-1
  [r]
  {:frep (frep-sphere r)
   :brep (brep-sphere r)})

(defn sphere-idea-2
  [r]
  (let [frep (frep-sphere r)
        brep (brep-sphere r)]
    (fn
      ([x y z] (frep x y z))
      ([u v] (brep u v)))))

(defn rect
  ([w h]
   (rect w h nil))
  ([w h col]
   [:rect {:class ["ln" (if col col "clr")]
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

(defn sc
  [sc & elems]
  (into [:g {:transform (scale-str sc)}] elems))

(defn mv
  [[x y] & elems]
  (into [:g {:transform (translate-str x y)}] elems))

(defn rot
  [r [x y] & elems]
  (into [:g {:transform (rotate-str r [x y])}] elems))

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

(defn wrap-list-once
  [s]
  (conj (drop-last s) (last s)))

(defn offset
  [pts d]
  (let [edges (cycle-pairs pts)
        opts (mapcat #(offset-edge % d) edges)
        oedges (every-other (cycle-pairs opts))
        edge-pairs (cycle-pairs oedges)]
    (wrap-list-once (map #(apply line-intersection %) edge-pairs))))

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
  (let [text (str (round (distance a b) 3))
        label-offset (* 0.225 (count text))
        label-angle (to-deg (angle-between-lines (-line a b) (-line [0 0] [1 0])))
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
     (mv (map - mid [label-offset 0]) (rot label-angle [label-offset 0] (sc 1.5 (label text)))))))

(defn dot
  [[x y]]
  [:circle {:class ["attn"]
            :cx x :cy y :r 0.125}])

(defn attn-ln
  [a b]
  [:polyline {:class ["attn-ln" "clr"]
              :points (pt-str [a b])}])

(defn attn-circle
  [[x y] r]
  [:circle {:class ["attn-ln" "clr"]
            :cx x :cy y :r r}])

(defn circular-pattern
  "Patterns n elements along an arc defined by angle."
  [[angle n rx ry] & elems]
  (let [delta (/ angle n)]
    (for [a (range n)]
      (rot (* a delta) [rx ry] elems))))

(def entity-defaults
  {:color "#2e3440"
   :fill "#a3be8c"})

(defn entity
  ([vl]
   (entity entity-defaults vl))
  
  ([attr-map vl]
   {:val vl
    :attrs (merge entity-defaults attr-map)}))

(defn polygon-2d
  "creates a closed 2d polygon tracing the list of points"
  [pts]
  (let [edges (map vec (partition 2 1 (concat 
                                       (range (count pts))
                                       [0])))]
    {:nodes (map entity pts)
     :edges (map entity edges) 
     :faces (list (entity (vec (range (count edges)))))}))

(defn regular-polygon-pts
  [r n]
  (let [angle (* 2 Math/PI (/ 1 n))]
    (for [step (range n)]
      [(* r (Math/cos (* step angle)))
       (* r (Math/sin (* step angle)))])))

(defn regular-polygon
  [r n]
  (polygon-2d (regular-polygon-pts r n)))

(defn make-vert-face
  [sk idx]
  (let [n (count (:edges sk))
        indices (vec (take (* 2 n) (cycle (range (* 2 n) (* 3 n)))))]
    [idx (get indices (inc idx)) (+ n idx) (get indices idx)]))

(defn extrude-
  [sk h]
  (let [nodes (map :val (:nodes sk))
        bnodes (map #(conj % 0) nodes)
        tnodes (map #(conj % h) nodes)
        xnodes (concat bnodes tnodes)

        nedges (count (:edges sk))
        bidx (concat (range nedges) [0])
        tidx (concat (range nedges (* 2 nedges)) [nedges])
        bedges (map :val (:edges sk))
        tedges (map vec (partition 2 1 tidx))
        medges (map vec (partition 2 (interleave bidx tidx)))
        xedges (concat bedges tedges medges)

        bfaces (map :val (:faces sk))
        mfaces (map #(make-vert-face sk %) (range (count nodes)))
        tfaces (list (vec (drop-last tidx)))
        xfaces (concat bfaces mfaces tfaces)]
    (-> sk
        (assoc :nodes (map entity xnodes))
        (assoc :edges (drop-last (map entity xedges)))
        (assoc :faces (map entity xfaces)))))

(defn shift-indices
  [entity shift-val]
  (let [shifter (fn [v]
                  (mapv #(+ shift-val %) v))]
    (update entity :val shifter)))

(defn join-two
  [a b]
  (let [anodes (:nodes a)
        aedges (:edges a)
        afaces (:faces a)
        bnodes (:nodes b)
        bedges (map #(shift-indices % (count anodes)) (:edges b))
        bfaces (map #(shift-indices % (count aedges)) (:faces b))
        unodes (concat anodes bnodes)
        uedges (concat aedges bedges)
        ufaces (concat afaces bfaces)]
    {:nodes unodes
     :edges uedges
     :faces ufaces}))

(defn join
  [& solids]
  (reduce join-two solids))

(defn scl [theta]
  [(Math/sin (Math/toRadians theta)) (Math/cos (Math/toRadians theta))])

(defn -rot-pt
  [[a b] theta]
  (let [[s-t c-t] (scl theta)]
    [(- (* a c-t) (* b s-t))
     (+ (* b c-t) (* a s-t))]))

(defn rot-pt
  [axis theta [x y z]]
  (cond
   (= axis :x) (into [x] (-rot-pt [y z] theta))
   (= axis :y) (apply #(into [] [%2 y %1]) (-rot-pt [z x] theta))
   (= axis :z) (into (-rot-pt [x y] theta) [z])))

(defn -rot
  [ro axis theta]
  (let [nodes (map :val (:nodes ro))
        rnodes (mapv (partial rot-pt axis theta) nodes)]
    (assoc ro :nodes (mapv entity rnodes))))

(defn rot-
  [ro [th-x th-y th-z]]
  (-> ro
    (-rot :z th-z)
    (-rot :y th-y)
    (-rot :x th-x)))

(defn mv-pt
  [delta pt]
  (mapv + delta pt))

(defn mv-
  [ro delta]
  (let [nodes (map :val (:nodes ro))
        rnodes (mapv (partial mv-pt delta) nodes)]
    (assoc ro :nodes (mapv entity rnodes))))

(defn sk->3d
  [sk]
  (let [nodes-2d (map :val (:nodes sk))
        nodes-3d (map #(entity (conj % 0)) nodes-2d)]
    (assoc sk :nodes nodes-3d)))

(defn face->edges
  [ro face]
  (let [edges (mapv :val (:edges ro))]
    (mapv #(get edges %) face)))

(defn edge->nodes
  [ro edge]
  (let [nodes (mapv :val (:nodes ro))]
    (mapv #(get nodes %) edge)))

(defn face->nodes
  [ro face]
  (let [edges (face->edges ro face)]
    (into [] (distinct (mapcat (partial edge->nodes ro) edges)))))

(def iso-euler-angles [30 45 0])
(def origin-angle-adjust-a [90 0 0])
(def origin-angle-adjust-b [0 90 0])
(defn orient-iso
  [ro]
  (-> ro
      (rot- origin-angle-adjust-a)
      (rot- origin-angle-adjust-b)
      (rot- iso-euler-angles)))

(defn orient-top
  [ro]
  (rot- ro [0 0 270]))

(defn orient-front
  [ro]
  (rot- ro [90 0 270]))

(defn orient-right
  [ro]
  (rot- ro [90 0 0]))

(defn draw-edges
  ([ro]
   (draw-edges ro orient-iso))
  
  ([ro orientation]
   (let [ro (orientation ro)
         nodes (map :val (:nodes ro))
         edges (:edges ro)]
     (for [edge edges]
       (let [col (:color (:attrs edge))
             [n0 n1] (:val edge)
             [xa ya _] (nth nodes n0)
             [xb yb _] (nth nodes n1)]
         (poly-path [[[xa ya] [xb yb]]]))))))

;; this was an older version of draw-edges
(defn obj?
  [item]
  (and (map? item)
       (and
        (contains? item :nodes)
        (contains? item :edges)
        (contains? item :faces))))

(defn get-nested-objects
  [ro]
  (filter seqable? ro))

(defn draw-edges-recursive
  [ro]
  (if (and (coll? ro) 
           (not (obj? ro)))
    (concat (map draw-edges-recursive ro))
    (draw-edges ro identity)))

(defn get-2d-pts
  [ro]
  (bb-center 
   (map #(into [] (take 2 %))
        (map :val (:nodes ro)))))

(defn get-move-coords
  [[w h sc] ro]
  (let [dc (get-2d-pts ro)
        wc (mapv #(* (/ % 2.0) sc) [w h])]
    (mapv - wc dc)))

(defn center-view
  [[w h sc] ro]
  (let [coords (get-move-coords [w h (/ 1 sc)] ro)]
    (mv coords (draw-edges-recursive ro))))

;; iso, top, front, right
(defn cad-view
  [[w h sc] descr ro]
  (let [qw (/ w 2.0)
        qh (/ h 2.0)]
    (quadrant-figure 
     [w h sc] 
     descr
     (center-view [qw qh sc] (orient-iso ro))
     (center-view [qw qh sc] (orient-top ro))
     (center-view [qw qh sc] (orient-front ro))
     (center-view [qw qh sc] (orient-right ro)))))
