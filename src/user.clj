(ns user
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as test]
            [clojure.repl :refer [doc]]))

;; 1.2
(s/def ::sum-of-two-largest-args (s/cat :a pos-int? :b pos-int? :c pos-int?))
(s/fdef sum-of-two-largest
  :args :user/sum-of-two-largest-args
  :ret integer?)
(defn sum-of-two-largest
  "computes the sum of squares of the two largest of three input integers"
  [a b c]
  (try (cond (<= a b c) (+ (* b b) (* c c))
             (<= c b a) (+ (* b b) (* a a))
             :else (+ (* a a) (* c c)))
       (catch ArithmeticException _
         0)))

(->> (test/check `sum-of-two-largest) test/summarize-results)

;; 1.6
(s/def ::compute-cube-root-args (s/cat :x pos-int? :y pos-int?))
(s/fdef compute-cube-root
  :args :user/compute-cube-root-args
  :ret ratio?)
(defn compute-cube-root [x y]
  (letfn [(good-enough? [guess]
            (< (abs (- (Math/pow guess 2))) 0.001))
          (improve-approx []
            (try
              (/ (+ (* 2 y) (/ x (* y y))))
              (catch ArithmeticException _
                1/3)))]
    (if (good-enough? y)
      y
      (improve-approx))))

(->> (test/check `compute-cube-root) test/summarize-results)


;; 1.8
(defn ackerman [x y]
  (cond (zero? y) 0
        (zero? x) (* 2 y)
        (= y 1) 2
        :else (recur (dec x)
                     (ackerman x (dec y)))))

(ackerman 2 4) ; => 65536
(ackerman 1 10) ; => 1024
(ackerman 3 3) ; => 65536