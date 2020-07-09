(ns stylo.parametric
  (:require [stylo.draw :as d]
            [clojure.spec.alpha :as s]
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

;; does it make sense to make a defmacro?
;; you could 'build up' the for loop
;; one range s-expr per parameter, as determined
;; by the # of steps the user gives

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

(defn frep-box [l w h]
  (fn [x y z]
    (max (- x l) (- (- l) x)
         (- y w) (- (- w) y)
         (- z h) (- (- h) z))))

(defn *c3d
  "calculates cross product of two 3d-vectors"
  [a b]
  (let [[a1 a2 a3] a
        [b1 b2 b3] b
        i (- (* a2 b3) (* a3 b2))
        j (- (* a3 b1) (* a1 b3))
        k (- (* a1 b2) (* a2 b1))]
    [i j k]))

(defn brep-line
  [a b]
  (fn [t]
    (let [t (float t)]
      (cond
        (< t 0.0) 0
        (> t 1.0) 1 
        (= t 0.0) a
        (= t 1.0) b
        :else
        (mapv + a (map * (map - b a) (repeat t)))))))

(defn brep-axis-aligned-rect
  [l w n]
  (let [u2 (/ l 2.0)
        v2 (/ w 2.0)
        u1 (- u2)
        v1 (- v2)
        [mx my mz] n]
    (fn [u v]
      (let [a (first ((brep-line [u1 v1 0] [u2 v1 0]) u))
            b (second ((brep-line [u1 v1 0] [u1 v2 0]) v))]
        (cond 
          (and (= 0 mx) (= 0 my)) [a b mz]
          (and (= 0 mx) (= 0 mz)) [a my b]
          (and (= 0 my) (= 0 mz)) [mx a b])))))

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

(defn brep-circle
  [r]
  (fn [t]
    (let [t (* 2 Math/PI t)
          x (* r (Math/cos t))
          y (* r (Math/sin t))]
      [x y 0])))

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

(comment
 ;; I forget how to do this...
 ;; look up examples (in scad-clj for instance)
 (defmulti render
   (fn [approach body] approach))

 (defmethod render :quad [_ body] "quad")
 (defmethod render :default [_ body] "default")
)

(defn render-dot
  [f]
  (let [step 0.025]
    (map d/dot (sample-2 f step step))))

(defn render-dot-curve
  [f]
  (let [step 0.025]
    (map d/dot (sample-1 f step))))

(defn render-slice
  [f]
  (let [step 0.025
        slices (slice f step step)]
    (map d/closed-path slices)))

(defn render-quad
  [f]
  (let [step 0.025
        mesh (quad-mesh f step step)]
    (map #(d/closed-path % "face") mesh)))
