(ns stylo.chunk
  (:require [stylo.draw :refer :all]))

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
