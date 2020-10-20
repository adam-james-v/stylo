(ns stylo.svg
  (:require [clojure.string :as s]
            [forge.proto :as f]
            #?(:cljs 
               [cljs.reader :refer [read-string]])))

(defn svg
  [[w h sc] & content]
  [:svg {:width w
         :height h
         :viewBox (str "-1 -1 " w " " h)
         :xmlns "http://www.w3.org/2000/svg"}
   [:g {:transform (str "scale(" sc ")")} content]])

(defn pt->str
  [pt]
  (apply str (interpose "," pt)))

(defn str->pt
  [s]
  (mapv read-string (s/split s #",")))

(defn points->str
  [pts]
  (apply str (interpose " " (map pt->str pts))))

(defn str->points
  [s]
  (mapv str->pt (s/split s #" ")))

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

(defn xf-kv->str
  [[k v]]
  (let [k (symbol k)
        v (apply list v)]
    (str k v)))

(defn str->xf-kv
  [s]
  (let [split (s/split s #"\(")
        key (keyword (first split))
        val (vec (read-string (str "(" (second split))))]
    [key val]))

(defn xf-map->str
  [m]
  (apply str (interpose "\n" (map xf-kv->str m))))

(defn str->xf-map
  [s]
  (if-let [s s]
    (into {} (map str->xf-kv (s/split-lines s)))
    {}))

(def svg-elements 
  #{:circle
    :ellipse
    :line
    :path
    :polygon
    :polyline
    :rect
    :text
    :g})

#?(:clj
   (defn check-svg-impl
     [multimethod]
     (println "Missing Implementation for:"
              (clojure.set/difference 
               svg-elements
               (into #{} (keys (methods multimethod)))))))

(defn circle
  [r]
  [:circle {:cx 0 :cy 0 :r r}])

(defn ellipse
  [rx ry]
  [:ellipse {:cx 0 :cy 0 :rx rx :ry ry}])

(defn line
  [[ax ay] [bx by]]
  [:line {:x1 ax :y1 ay :x2 bx :y2 by}])

(defn path
  [d]
  [:path {:d d
          :fill-rule "evenodd"}])

(defn polygon
  [pts]
  [:polygon {:points (points->str pts)}])

(defn polyline
  [pts]
  [:polyline {:points (points->str pts)}])

(defn rect
  [w h]
  [:rect {:width w :height h :x (/ w -2.0) :y (/ h -2.0) :z 0}])

(defn text
  [text]
  (let [char-w 9.625
        char-h 10
        n-chars (count text)
        x (/ (* n-chars char-w) -2.0)
        y (/ char-h 2.0)]
    [:text {:x (/ (* n-chars char-w) -2.0)
            :y (/ char-h 2.0)
            :transform (xf-map->str {:rotate [0 (- x) (- y)]})
            :style {:font-family "monospace"
                    :font-size 16}} text]))

(defn g
  [& content]
  (into [:g {}] content))

(declare color-element)

;; types of paths line, arc, quadratic, cubic
(defn path->pts
  [s]
  (as-> s s
    (s/replace s #"Z" "") ;; removes Z at end of path
    (s/split s #"\s") ;; split string at spaces
    (mapcat #(s/split % #"[A-Za-z]") s) ;;splits on alpha chars
    (filter #(not (= % "")) s)
    (map read-string s)
    (vec (map vec (partition 2 s)))))

(defn path-type
  [s]
  (cond 
    (s/includes? s "L") :line
    (s/includes? s "l") :line
    (s/includes? s "C") :cubic
    (s/includes? s "c") :relative-cubic
    (s/includes? s "Q") :quadratic
    (s/includes? s "A") :arc))

(defn closed?
  [s]
  (= \Z (last s)))

(defmulti path-string->path
  (fn [s]
    (path-type s)))

(defmethod path-string->path :default
  [s]
  {:type (path-type s)
   :closed (closed? s)
   :pts (path->pts s)})

(defmethod path-string->path :arc
  [s]
  (let [xs (-> s
               (s/replace #"[A-Za-z]" "")
               (s/split #"\s")
               (#(filter (complement s/blank?) %)))
        [p1x p1y rx ry x-deg lg sw p3x p3y] xs]
    {:type :arc
     :closed (closed? s)
     :p1 [p1x p1y]
     :p3 [p3x p3y]
     :rx rx
     :ry ry
     :x-deg x-deg
     :lg lg
     :sw sw}))

(defn -str
  [leader & pts]
  (apply str (interpose " " (concat [leader] (flatten pts)))))

(defmulti path->path-string
  (fn [p]
    (:type p)))

(defmethod path->path-string :line
  [{:keys [closed pts]}]
  (let [[m & pts] pts]
    (str 
     (-str "M" m) " "
     (apply str (interpose " " (map (partial -str "L") pts)))
     (when closed " Z"))))

(defmethod path->path-string :quadratic
  [{:keys [closed pts]}]
  (let [[p1 c p2 & pts] pts]
    (str
     (-str "M" p1) " "
     (-str "Q" c p2) " "
     (apply str (interpose " "
                       (map #(apply (partial -str "T") %) (partition 2 pts))))
     (when closed " Z"))))

(defmethod path->path-string :cubic
  [{:keys [closed pts]}]
  (let [[p1 c1 c2 p2  & pts] pts]
    (str
     (-str "M" p1) " "
     (-str "C" c1 c2 p2) " "
     (apply str (interpose 
                 " " 
                 (map #(apply (partial -str "S") %) (partition 2 pts))))
     (when closed " Z"))))

(defmethod path->path-string :relative-cubic
  [{:keys [closed pts]}]
  (let [[p1 c1 c2 p2  & pts] pts]
    (str
     (-str "M" p1) " "
     (-str "c" c1 c2 p2) " "
     (apply str (interpose 
                 " " 
                 (map #(apply (partial -str "") %) (partition 2 pts))))
     (when closed " Z"))))

(defmethod path->path-string :arc
  [{:keys [p1 p3 rx ry x-deg lg sw closed]}]
  (str
   (-str "M" p1) " "
   (-str "A" [rx ry] [x-deg lg sw] p3)
   (when closed " Z")))

(defn path-polygon-str
  [[m & pts]]
  (str 
   (-str "M" m) " "
   (apply str 
          (interpose " " (map (partial -str "L") pts)))
   " Z"))

(defn path-polyline-str
  [[m & pts]]
  (str 
   (-str "M" m) " "
   (apply str 
          (interpose " " (map (partial -str "L") pts)))))

(defn path-polygon
  [& paths]
  (let [paths (map path-polygon-str paths)]
    (path (apply str (interpose "\n" paths)))))

(defn path-polyline
  [& paths]
  (let [paths (map path-polyline-str paths)]
    (path (apply str (interpose "\n" paths)))))

(declare style-element)
(defn cubic-bezier-str
  [[x1 y1] [cx1 cy1] [cx2 cy2] [x y]]
  (let [m-str (str "M " x1 " " y1 " ")
        c-str (apply str (interpose " " ["C" cx1 cy1 cx2 cy2 x y]))]
    (str m-str c-str)))

(defn s-bezier-str
  [[cx1 cy1] [x y]]
  (apply str (interpose " " ["S" cx1 cy1 x y])))

(defn cubic-bezier
  [pts]
  (let [curve1 (apply cubic-bezier-str (take 4 pts))
        s-curves (map #(apply s-bezier-str %)
                      (partition 2 (drop 4 pts)))]
    (path 
     (apply str (interpose " " (cons curve1 s-curves))))))

#_(defn cubic-bezier-debug
  [pts]
  (let [curve1 (apply cubic-bezier-str (take 4 pts))
        s-curves (map #(apply s-bezier-str %)
                      (partition 2 (drop 4 pts)))]
    (g
     (style-element
      {:stroke "black"
       :stroke-width 1}
      (g
       (map #(translate % (circle 2)) pts)
       (polyline pts)))
     (path 
      (apply str (interpose " " (cons curve1 s-curves)))))))

(defn arc-str
  [rx ry x-deg lg sw x y]
  (apply str (interpose " " ["a" rx ry x-deg lg sw x y])))

;; arc drawing can be done in a few ways.
;; could implement different drawing methods w/ defmethod,
;; dispatch on :key OR on 'shape' of the args?

(defn large-arc-flag
  [p1 p2 p3]
  (let [[p1b p2b p3b] (map #(conj % 0) [p1 p2 p3])
        c (drop-last (f/center-from-pts p1b p2b p3b))
        a1 (f/angle-from-pts p1 c p2)
        a2 (f/angle-from-pts p2 c p3)
        a (+ a1 a2)]
    (if (< 180 a) 1 0)))

;; figure out how to properly set sweep flag.
;; this breaks when p1 and p3 are swapped (even though 
;; the arc should be drawn the same.. it also breaks
;; when p2 is in Q4

(defn sweep-flag
  [p1 p2 p3]
  (let [[p1b p2b p3b] (map #(conj % 0) [p1 p2 p3])
        c (drop-last (f/center-from-pts p1b p2b p3b))]
    (if (or (> (second p2) (second c))
            (> (first p2) (first c))) 0 1)))

(declare circle-by-pts)
(declare translate)
(declare rotate)
(declare scale)
(defn arc
  [p1 p2 p3]
  (let [[p1b p2b p3b] (map #(conj % 0) [p1 p2 p3]) 
        r (f/radius-from-pts p1b p2b p3b)
        m-str (apply str (interpose " " (cons "M" p1)))
        a-str (apply str 
                     (interpose " " (concat ["A" r r 0 
                                             (large-arc-flag p1 p2 p3)
                                             (sweep-flag p1 p2 p3)] p3)))]
    (g
     (circle-by-pts p1 p2 p3)
     (path (apply str (interpose " " [m-str a-str]))))))

(defn merge-paths
  "Merges svg <path> elements together, keeping props from last path in the list."
  [& paths]
  (let [props (second (last paths))
        d (apply str (interpose "\n" (map #(get-in % [1 :d]) paths)))]
    [:path (assoc props :d d)]))

(defn join-paths
  [& paths]
  (let [props (second (last paths))
        strings (concat [(get-in (first paths) [1 :d])] 
                        (mapv #(s/replace (get-in % [1 :d]) #"M" "L") (rest paths)))
        d (apply str (interpose "\n" strings))]
    [:path (assoc props :d d)]))

(defn close-path
  [[k props]]
  (let [path-string (:d props)]
    [k (assoc props :d (str path-string " Z"))]))

(defmulti translate-element 
  (fn [_ element]
    (first element)))

(defmethod translate-element :circle
  [[x y] [k props]]
  (let [xf (str->xf-map (get props :transform "rotate(0 0 0)"))
        cx (:cx props)
        cy (:cy props)
        new-xf (-> xf
                   (assoc-in [:rotate 1] (+ x cx))
                   (assoc-in [:rotate 2] (+ y cy)))
        new-props (-> props
                      (assoc :transform (xf-map->str new-xf))
                      (update :cx + x)
                      (update :cy + y))]
    [k new-props]))

(defmethod translate-element :ellipse
  [[x y] [k props]]
  (let [xf (str->xf-map (get props :transform "rotate(0 0 0)"))
        cx (:cx props)
        cy (:cy props)
        new-xf (-> xf
                   (assoc-in [:rotate 1] (+ x cx))
                   (assoc-in [:rotate 2] (+ y cy)))
        new-props (-> props
                      (assoc :transform (xf-map->str new-xf))
                      (update :cx + x)
                      (update :cy + y))]
    [k new-props]))

(defmethod translate-element :line
  [[x y] [k props]]
  (let [new-props (-> props
                      (update :x1 + x)
                      (update :y1 + y)
                      (update :x2 + x)
                      (update :y2 + y))]
    [k new-props]))

(defmethod translate-element :path
  [[x y] [k props]]
  (let [path-strings (s/split-lines (:d props))
        paths (map path-string->path path-strings)
        new-paths (for [path paths]
                    (let [xpts (map #(map + [x y] %) (:pts path))]
                      (path->path-string (assoc path :pts xpts))))
        new-props (assoc props :d (apply str (interpose "\n" new-paths)))]
    [k new-props]))

(defmethod translate-element :polygon
  [[x y] [k props]]
  (let [points (str->points (:points props))
        new-points (points->str (map #(map + [x y] %) points))
        new-props (assoc props :points new-points)]
    [k new-props]))

(defmethod translate-element :polyline
  [[x y] [k props]]
  (let [points (str->points (:points props))
        new-points (points->str (map #(map + [x y] %) points))
        new-props (assoc props :points new-points)]
    [k new-props]))

(defmethod translate-element :rect
  [[x y] [k props]]
  (let [xf (str->xf-map (get props :transform "rotate(0 0 0)"))
        cx (+ (:x props) (/ (:width props) 2.0))
        cy (+ (:y props) (/ (:height props) 2.0))
        new-xf (-> xf
                   (assoc-in [:rotate 1] (+ x cx))
                   (assoc-in [:rotate 2] (+ y cy)))
        new-props (-> props
                      (assoc :transform (xf-map->str new-xf))
                      (update :x + x)
                      (update :y + y))]
    [k new-props]))

(defmethod translate-element :text
  [[x y] [k props text]]
  (let [xf (str->xf-map (get props :transform "rotate(0 0 0)"))
        new-xf (-> xf
                   (update-in [:rotate 1] + x)
                   (update-in [:rotate 2] + y))
        new-props (-> props
                      (assoc :transform (xf-map->str new-xf))
                      (update :x + x)
                      (update :y + y))]
    [k new-props text]))

(defmethod translate-element :g
  [[x y] [k props & content]]
  (let [xf (str->xf-map (:transform props))
        new-xf (-> xf
                   (update :translate (fnil #(map + [x y] %) [0 0])))
        new-props (assoc props :transform (xf-map->str new-xf))]
    (into [k new-props] content)))

(defn element? [item]
  (svg-elements (first item)))

(defn translate
  [[x y] & elems]
  (let [elem (first elems)
        elems (rest elems)]
    (when elem
      (cond
        (and (element? elem) (= 0 (count elems)))
        (translate-element [x y] elem)
        
        (and (element? elem) (< 0 (count elems)))
        (concat
         [(translate-element [x y] elem)]
         [(translate [x y] elems)])
      
        :else
        (recur [x y] (concat elem elems))))))

;; this is the 'old' way.
(defn translate-g
  [[x y] & elems]
  (into [:g {:transform (translate-str x y)}] elems))

(defn rotate-element-by-transform
  [deg [k props content]]
  (let [xf (str->xf-map (get props :transform "rotate(0 0 0)"))
        new-xf (-> xf
                   (update-in [:rotate 0] + deg))
        new-props (assoc props :transform (xf-map->str new-xf))]
    [k new-props content]))

(defn rotate-pt
  [deg [x y]]
  (let [c (Math/cos (f/to-rad deg))
        s (Math/sin (f/to-rad deg))]
    [(- (* x c) (* y s))
     (+ (* x s) (* y c))]))

(defmulti rotate-element
  (fn [_ element]
    (first element)))

(defmethod rotate-element :circle
  [deg [k props]]
  (rotate-element-by-transform deg [k props]))

(defmethod rotate-element :ellipse
  [deg [k props]]
  (rotate-element-by-transform deg [k props]))

(defn move-pt
  [mv pt]
  (mapv + pt mv))

(defn rotate-pt-around-center
  [deg center pt]
  (->> pt
       (move-pt (map - center))
       (rotate-pt deg)
       (move-pt center)))

(defmethod rotate-element :line
  [deg [k props]] 
  (let [pts [[(:x1 props) (:y1 props)] [(:x2 props) (:y2 props)]]
        center (f/bb-center-2d pts)
        [[x1 y1] [x2 y2]]  (map (partial rotate-pt-around-center deg center) pts)
        new-props (-> props
                      (assoc :x1 x1)
                      (assoc :y1 y1)
                      (assoc :x2 x2)
                      (assoc :y2 y2))]
    [k new-props]))

(defmethod rotate-element :path
  [deg [k props]]
  (let [path-strings (s/split-lines (:d props))
        paths (map path-string->path path-strings)
        center (f/bb-center-2d (apply concat (map :pts paths)))
        new-paths (for [path paths]
                    (let [xf (partial rotate-pt-around-center deg center)
                          xpts (map xf (:pts path))]
                      (path->path-string (assoc path :pts xpts))))
        new-props (assoc props :d (apply str (interpose "\n" new-paths)))]
    [k new-props]))

(defmethod rotate-element :polygon
  [deg [k props]]
  (let [points (str->points (:points props))
        center (f/bb-center-2d points)
        new-points (points->str
                    (map 
                     (partial rotate-pt-around-center deg center)
                     points))
        new-props (assoc props :points new-points)]
    [k new-props]))

(defmethod rotate-element :polyline
  [deg [k props]]
  (let [points (str->points (:points props))
        center (f/bb-center-2d points)
        new-points (points->str
                    (map 
                     (partial rotate-pt-around-center deg center)
                     points))
        new-props (assoc props :points new-points)]
    [k new-props]))

(defmethod rotate-element :rect
  [deg [k props]]
  (rotate-element-by-transform deg [k props]))

(defmethod rotate-element :text
  [deg [k props text]]
  (rotate-element-by-transform deg [k props text]))

(defmethod rotate-element :g
  [deg [k props & content]]
  (rotate-element-by-transform deg [k props content]))

(defn rotate
  [deg & elems]
  (let [elem (first elems)
        elems (rest elems)]
    (when elem
      (cond
        (and (element? elem) (= 0 (count elems)))
        (rotate-element deg elem)
        
        (and (element? elem) (< 0 (count elems)))
        (concat
         [(rotate-element deg elem)]
         [(rotate deg elems)])
        
        :else
        (recur deg (concat elem elems))))))

;; old approach
(defn rotate-g
  [r [x y] & elems]
  (into [:g {:transform (rotate-str r [x y])}] elems))

(defn scale-element-by-transform
  [[sx sy] [k props & content]]
  (let [xf (str->xf-map (:transform props))
        new-xf (-> xf
                   (update :scale (fnil #(map * [sx sy] %) [1 1])))
        new-props (assoc props :transform (xf-map->str new-xf))]
    [k new-props] content))

(defmulti scale-element 
  (fn [_ element]
    (first element)))

;; transforms are applied directly to the properties of shapes.
;; I have scale working the same way. One issue is that scaling a circle
;; turns it into an ellipse. This impl WILL change the shape to ellipse if non-uniform scaling is applied.

(defmethod scale-element :circle
  [[sx sy] [k props]]
  (let [circle? (= sx sy)
        r (:r props)
        new-props (if circle?
                    (assoc props :r (* r sx))
                    (-> props
                        (dissoc :r)
                        (assoc :rx (* sx r))
                        (assoc :ry (* sy r))))
        k (if circle? :circle :ellipse)]
    [k new-props]))

(defmethod scale-element :ellipse
  [[sx sy] [k props]]
  (let [new-props (-> props
                      (update :rx #(* sx %))
                      (update :ry #(* sy %)))]
    [k new-props]))

;; find bounding box center
;; translate bb-center to 0 0
;; scale all x y values by * [sx sy]
;; translate back to original bb-center

(defmethod scale-element :line
  [[sx sy] [k props]]
  (let [[cx cy] (f/bb-center-2d [[(:x1 props) (:y1 props)]
                              [(:x2 props) (:y2 props)]])
        new-props (-> props
                      (update :x1 #(+ (* (- % cx) sx) cx))
                      (update :y1 #(+ (* (- % cy) sy) cy))
                      (update :x2 #(+ (* (- % cx) sx) cx))
                      (update :y2 #(+ (* (- % cy) sy) cy)))]
    [k new-props]))

(defn scale-pt-from-center
  [[cx cy] [sx sy] [x y]]
  [(+ (* (- x cx) sx) cx)
   (+ (* (- y cy) sy) cy)])

(defmethod scale-element :path
  [[sx sy] [k props]]
  (let [path-strings (s/split-lines (:d props))
        paths (map path-string->path path-strings)
        center (f/bb-center-2d (apply concat (map :pts paths)))
        new-paths (for [path paths]
                    (let [xf (partial scale-pt-from-center center [sx sy])
                          xpts (map xf (:pts path))]
                      (path->path-string (assoc path :pts xpts))))
        new-props (assoc props :d (apply str (interpose "\n" new-paths)))]
    [k new-props]))

(defmethod scale-element :polygon
  [[sx sy] [k props]]
  (let [points (str->points (:points props))
        center (f/bb-center-2d points)
        new-points (points->str
                    (map 
                     (partial scale-pt-from-center center [sx sy])
                     points))
        new-props (assoc props :points new-points)]
    [k new-props]))

(defmethod scale-element :polyline
  [[sx sy] [k props]]
  (let [points (str->points (:points props))
        center (f/bb-center-2d points)
        new-points (points->str
                    (map 
                     (partial scale-pt-from-center center [sx sy])
                     points))
        new-props (assoc props :points new-points)]
    [k new-props]))

(defmethod scale-element :rect
  [[sx sy] [k props]]
  (let [cx (+ (:x props) (/ (:width props) 2.0))
        cy (+ (:y props) (/ (:height props) 2.0))
        w (* sx (:width props))
        h (* sy (:height props))
        new-props (-> props
                      (assoc :width w)
                      (assoc :height h)
                      (update :x #(+ (* (- % cx) sx) cx))
                      (update :y #(+ (* (- % cy) sy) cy)))]
    [k new-props]))

(defmethod scale-element :text
  [[sx sy] [k props text]]
  (let [xf (str->xf-map (get props :transform "rotate(0 0 0)"))
        cx (get-in xf [:rotate 1])
        cy (get-in xf [:rotate 2])
        x (+ (* (- (:x props) cx) sx) cx)
        y (+ (* (- (:y props) cy) sy) cy)
        new-xf (-> xf
                   (assoc-in [:rotate 1] (- x))
                   (assoc-in [:rotate 2] (- y)))
        new-props (-> props
                      (assoc :transform (xf-map->str new-xf))
                      (assoc :x x)
                      (assoc :y y)
                      (update-in [:style :font-size] #(* % sx)))]
    [k new-props text]))

(defmethod scale-element :g
  [[sx sy] [k props & content]]
  (let [xf (str->xf-map (:transform props))
        new-xf (-> xf
                   (update :scale (fnil #(map * [sx sy] %) [1 1])))
        new-props (assoc props :transform (xf-map->str new-xf))]
    (into [k new-props] content)))

(defn scale
  [sc & elems]
  (let [[sx sy] (if (coll? sc) sc [sc sc])
        elem (first elems)
        elems (rest elems)]
    (when elem
      (cond
        (and (element? elem) (= 0 (count elems)))
        (scale-element [sx sy] elem)
        
        (and (element? elem) (< 0 (count elems)))
        (concat
         [(scale-element [sx sy] elem)]
         [(scale [sx sy] elems)])
        
        :else
        (recur [sx sy] (concat elem elems))))))

;; this is the old method
(defn scale-g
  [sc & elems]
  (into [:g {:transform (scale-str sc)}] elems))

(defn style-element
  [style [k props & content]]
  [k (merge props style) content])

(defn color-element
  [color [k props & content]]
  (let [color {:fill "none"
               :stroke color}]
    [k (merge props color) content]))

(defn color
  [style & elems]
  (let [elem (first elems)
        elems (rest elems)]
    (when elem
      (cond
        (and (element? elem) (= 0 (count elems)))
        (color-element style elem)
        
        (and (element? elem) (< 0 (count elems)))
        (concat
         [(color-element style elem)]
         [(color style elems)])
        
        :else
        (recur style (concat elem elems))))))

(defn arrow
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
               :points (points->str [a b])}]])

(defn circle-by-pts
  [p1 p2 p3]
  (let [[p1 p2 p3] (map #(conj % 0) [p1 p2 p3]) 
        r (f/radius-from-pts p1 p2 p3)
        c (drop-last (f/center-from-pts p1 p2 p3))]
    (style-element
     {:fill "none"
      :stroke "gray"
      :stroke-width 1}
     (g
      (translate c (circle r))
      (translate c (circle 2))
      (translate (drop-last p1) (circle 2))
      (translate (drop-last p2) (circle 2))
      (translate (drop-last p3) (circle 2))))))

(defn figure
  [[w h sc fig-num] descr & content]
  [:div.figure
   (svg [w h sc] content)
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
    (assoc-in (svg [w h sc] content) [1 :viewBox] view-str)))

(defn axes-2d
  []
  [:g#axes
   (map
    (partial style-element {:stroke-width 1})
    [(color "#a3be8c" (line [0 -1000] [0 1000]))
     (color "#bf616a" (line [-1000 0] [1000 0]))])])

;; this fn will tell you the parameter that correspondss to the distance along the line

(comment
  (defn d->t
    [f d]
    (let [l (f/distance (f 0) (f 1))]
      (/ d l)))

  ;; fn will tell you the distance along the line that parameter's point is.
  (defn t->d
    [f t]
    (f/distance (f 0) (f t)))

  (defn dimension
    [a b]
    (let [text (str (round (f/distance a b) 3))
          label-offset (* 0.225 (count text))
          label-angle (f/to-deg (f/angle-between-lines-2d (f/brep-line a b) (f/brep-line [0 0] [1 0])))
          [ao bo] (f/offset-edge [a b] 2)
          mid (f/bb-center-2d [ao bo])
          arw-a (f/brep-line mid ao)
          arw-b (f/brep-line mid bo)
          arw-t (- 1 (d->t arw-a 0.5))
          mid-t (d->t arw-a (* 1.75 label-offset))
          la (f/brep-line a ao)
          lb (f/brep-line b bo)
          [lat1 lat2] (map (partial d->t la) [0.5 2.5])
          [lbt1 lbt2] (map (partial d->t lb) [0.5 2.5])]
      (list
       (arrow (arw-a mid-t) (arw-a arw-t))
       (arrow (arw-b mid-t) (arw-b arw-t))
       (line (la lat1) (la lat2))
       (line (lb lbt1) (lb lbt2))
       (translate (map - mid [label-offset 0]) (rotate label-angle [label-offset 0] (scale 1.5 (label text)))))))

  )
