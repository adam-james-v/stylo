(ns stylo.draw
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [clojure.spec.gen.alpha :as gen]
            [clojure.test :as test]
            [stylo.geom :refer [round
                                to-deg
                                to-rad
                                bb-center
                                distance
                                perpendicular
                                normalize
                                det
                                *c3d]]
            [stylo.svg :refer [svg
                               circle
                               ellipse
                               line
                               path
                               path-polygon
                               path-polyline
                               polygon
                               polyline
                               rect
                               text
                               scale
                               translate
                               rotate
                               arrow]]))

(defn label [t]
  (text t))

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

(defn translate-p
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
     (arrow (arw-a mid-t) (arw-a arw-t))
     (arrow (arw-b mid-t) (arw-b arw-t))
     (line (la lat1) (la lat2))
     (line (lb lbt1) (lb lbt2))
     (translate (map - mid [label-offset 0]) (rotate label-angle [label-offset 0] (scale 1.5 (label text)))))))

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
         (path [[xa ya] [xb yb]]))))))

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
    (translate coords (draw-edges-recursive ro))))

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
