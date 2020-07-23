(ns stylo.draw-test
  (:require [clojure.test :refer [deftest is are testing run-tests]]
            [stylo.draw :as draw]))

;; when using the REPL, (load-file "test/stylo/draw_test.cljc")
;; will load this file and run the following s-exprs, causing
;; every test to run and display in your REPL, without needing
;; to change out of your working ns

(load-file "src/stylo/draw.cljc")
(run-tests)

(deftest path-str-test
  (is (= (draw/path-str [[0 0] [1 1] [2 2]]) "M0 0 L1 1 L2 2 Z")
      "unexpected path string."))
