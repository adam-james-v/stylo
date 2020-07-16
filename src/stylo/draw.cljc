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

(defn sc
  [sc & elems]
  (into [:g {:transform (scale-str sc)}] elems))

(defn mv
  [[x y] & elems]
  (into [:g {:transform (translate-str x y)}] elems))

(defn rot
  [r [x y] & elems]
  (into [:g {:transform (rotate-str r [x y])}] elems))

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
     (mv (map - mid [label-offset 0]) (rot label-angle [label-offset 0] (sc 1.5 (label text)))))))

(defn dot
  [[x y]]
  [:circle {:class ["attn"]
            :cx x :cy y :r 0.125}])

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

(defn parametric-circle
  [r]
  (fn [t]
    (let [t (* 2 Math/PI t)
          x (* r (Math/cos t))
          y (* r (Math/sin t))]
      [x y])))

(defn circle
  [r]
  (let [circle-fn (parametric-circle r)]
    (polygon-2d (map circle-fn (range 0 1 0.025)))))

(defn parametric-ellipse
  [rx ry]
  (fn [t]
    (let [t (* 2 Math/PI t)
          x (* rx (Math/cos t))
          y (* ry (Math/sin t))]
      [x y])))

(defn ellipse
  [rx ry]
  (let [ellipse-fn (parametric-ellipse rx ry)]
    (polygon-2d (map ellipse-fn (range 0 1 0.025)))))

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

(def axes
  {:nodes (map entity [[0 0 0]
                       [1 0 0]
                       [0 1 0]
                       [0 0 1]])
   :edges (map #(apply entity %) [[{:color "red"} [0 1]]
                                  [{:color "green"} [0 2]]
                                  [{:color "blue"} [0 3]]])})

(defn ln-col
  [a b col]
  [:polyline.clr {:stroke-linecap "round"
                  :stroke col
                  :stroke-width "2"
                  :points (pt-str [a b])}])

(defn shape-col
  [pts col]
  [:polygon {:stroke "slategray"
             :fill col
             :stroke-width "2"
             :points (pt-str pts)}])

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
         (poly-path [[[xa ya] [xb yb]]])
         #_(ln-col [xa ya] [xb yb] col))))))

(defn all-true?
  [l]
  (let [s (into #{} l)]
    (if (= 2 (count s))
      false
      (true? (first s)))))

(defn all-false?
  [l]
  (let [s (into #{} l)]
    (if (= 2 (count s))
      false
      (false? (first s)))))

(defn loop-masks
  [edges]
  (let [indices (map first edges)]
    (for [idx indices]
      (mapv #(= idx (last %)) edges))))

(defn find-loops
  ([edges]
   (find-loops edges []))
  ([edges acc]
   (let [idx (first (first edges))
         mask (mapv #(= idx (last %)) edges)
         no-loops (all-false? mask)
         n-edges (inc (count (take-while false? mask)))]
     (if no-loops
       acc
       (recur 
        (drop n-edges edges)
        (conj acc (take n-edges edges)))))))

(defn loop-between
  [e1 e2]
  (let [[n1 n2] e1
        [n4 n3] e2]
    (list [n1 n2]
          [n2 n3]
          [n3 n4]
          [n4 n1])))

(defn make-loops
  [edges]
  (let [edges (concat edges [(first edges)])
        pairs (partition 2 1 edges)]
    (map #(apply loop-between %) pairs)))

(defn get-loops
  [edges]
  (let [#_edges #_(drop-last edges) ;;error in extrude adds extra edge at end of list.... fix 
        found (find-loops edges)
        to-remove (zipmap (apply concat found) (range (count (apply concat found))))
        remaining (drop-while #(contains? to-remove %) edges)
        made (make-loops remaining)]
    found #_(concat found made)))

(defn loop->pts
  [ro loop]
  (let [nodes (mapv :val (:nodes ro))
        indices (map first loop)
        pts (mapv #(get nodes %) indices)]
    pts))

(defn draw-loops
  [ro]
  (let [nodes (map :val (:nodes ro))
        edges (:edges ro)
        loops (get-loops (map :val edges))
        paths (map (partial loop->pts ro) loops)]
    (for [path paths #_(take 6 paths)]
      (closed-path path))))

;; rename this? widget, chunk, solid, something else...
(defn object?
  [item]
  (and (map? item)
       (and
        (contains? item :nodes)
        (contains? item :edges)
        (contains? item :faces))))

;; object looks like:
;; {:nodes [] :edges [] :faces []}

;; asm looks like:
;; [object object object ..]

(defn get-nested-objects
  [ro]
  (filter seqable? ro))

(defn draw-edges-recursive
  [ro]
  (if (and (coll? ro) 
           (not (object? ro)))
    (concat (map draw-edges-recursive ro))
    (draw-edges ro identity)))

(defn draw-faces
  ([ro]
   (draw-faces [ro orient-iso]))

  ([ro orientation]
   (let [ro (orientation ro)
         nodes (map :val (:nodes ro))
         faces (:faces ro)]
     (for [face faces]
       (let [fill (:fill (:attrs face))
             pts-3d (face->nodes ro (:val face))
             pts-2d (map #(take 2 %) pts-3d)]
         (shape-col pts-2d fill))))))

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
