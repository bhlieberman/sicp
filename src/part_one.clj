(ns part-one
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
(s/def ::ackerman-args (s/cat :x (s/int-in 1 10) :y (s/int-in 1 10)))
(s/fdef ackerman
  :args :user/ackerman-args
  :ret int?)
(defn ackerman [x y]
  (cond (zero? y) 0
        (zero? x) (* 2 y)
        (= y 1) 2
        :else (recur (dec x)
                     (ackerman x (dec y)))))

(ackerman 2 4) ; => 65536
(ackerman 1 10) ; => 1024
(ackerman 3 3) ; => 65536

(->> (test/check `ackerman) test/summarize-results) ; => StackOverflowError

;; 1.9
(defn count-coins-iter [amount [& coins]]
  (let [denoms {:p 1 :n 5 :d 10 :q 25}]
    (apply min (map #(- amount %) (vals denoms)))))

;; 1.11
(defn fast-exp [b n]
  (cond (zero? n) 1
        (even? n) (Math/pow (fast-exp b (/ n 2)) 2)
        :else (* b (fast-exp b (- n 1)))))

(defn fast-exp-iter [a b n]
  (let [inv (* a (Math/pow b n))]
    (cond (zero? n) 1
          (even? n) (Math/pow (fast-exp-iter a b (/ n 2)) 2)
          :else (* b (fast-exp-iter a b (- n 1))))))

(time (fast-exp 2 3))

;; 1.16
(defn find-divisor [n test-div]
  (cond (> (Math/pow test-div 2) n) n
        (zero? (mod n test-div)) test-div
        :else (recur n (+ test-div 1))))

(defn smallest-divisor [n]
  (find-divisor n 2))

(smallest-divisor 19999)

;; 1.23
(defn simpsons-rule [f a b n]
  (let [h (/ (- b a) n)
        y (fn [k] (f a (+ a (* k h))))]
    (* (/ h 3)
       (+  (y n) (reduce
                  (fn [acc n] (cond (even? n) (+ acc (* 2 (y n)))
                                    (odd? n) (+ acc (* 4 (y n))))) 0
                  (range n))))))

(defn sum [term a next b]
  (if (> a b)
    0
    (+ (term a) (sum term (next a) next b))))

(defn sum-cubes [a b]
  (letfn [(cube [x] (Math/pow x 3))]
    (sum cube a #'clojure.core/inc b)))

(simpsons-rule sum-cubes 1 10 10000000)