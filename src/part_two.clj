(ns part-two
  (:refer-clojure :exclude [reverse]))

;; 2.1
(defn make-rat+ [a b]
  (cond (neg? a) (cons a (list b))
        (and (pos? a) (pos? b)) (cons a (list b))))

(make-rat+ -130 -13)

;; 2.2
(deftype Point [x y])

(deftype Segment [^Point p1 ^Point p2])

(defprotocol Segmentable
  (make-segment [this [x1 y1] [x2 y2]])
  (start-point [this])
  (end-point [this])
  (midpoint [this]))

(extend-protocol Segmentable
  Segment
  (make-segment [_ [x1 y1] [x2 y2]]
    (->Segment {:x x1 :y y1} {:x x2 :y y2}))

  (start-point [this]
    (let [{:keys [x y]} (.p1 this)]
      (cons x (list y))))

  (end-point [this]
    (let [{:keys [x y]} (.p2 this)]
      (cons x (list y))))

  (midpoint [this]
    (let [{:keys [x y]} (merge-with - (.p1 this) (.p2 this))]
      (/ x y))))

(def s (->Segment {:x 1 :y 3} {:x 5 :y 7}))

;; 2.4
(defn exp-a [a] (Math/pow 2 a))
(defn exp-b [b] (Math/pow 3 b))

(defn cons+ [a b] (fn [m] (m a b)))

(defn car [z] (z (fn [p _] p)))

(defn cdr [z] (z (fn [_ q] q)))

#_(exp-a (cons+ 2 3))

;; 2.1.4
(defn intadd [x y]
  (let [[min-x min-y] (map #(apply min %) [x y])
        [max-x max-y] (map #(apply max %) [x y])]
    (range (+ min-x min-y) (+ max-x max-y))))

(defn intmul [x y]
  (let [[min-x min-y] (map #(apply min %) [x y])
        [max-x max-y] (map #(apply max %) [x y])
        p1 (* min-x min-y) p2 (* min-x max-y)
        p3 (* max-x min-y) p4 (* max-x max-y)]
    (range (min p1 p2 p3 p4)
           (max p1 p2 p3 p4))))

(defn intdiv [x y]
  (intmul x (range (/ 1 (apply max y))
                   (/ 1 (apply min y)))))

(intdiv (range 1 5) (range 5 10))

;; 2.16

(defn last+ [xs]
  (cond (zero? (count xs)) []
        (= 1 (count xs)) '(xs)
        :else (recur (rest xs))))

;; 2.17

(defn reverse [xs]
  (loop [[x & more :as all] xs
         acc []]
    (if all
      (recur more (conj acc x))
      acc)))

;; 2.18

(defn square-list [coll]
  (reduce (fn [acc x] (conj acc (* x x))) [] coll))

;; 2.3

;; Huffman trees

(defn huffman [tree-repr]
  (reduce (fn [[fst snd] [k v]] [(conj fst k) (+ snd v)])
          [#{} 0] tree-repr))

(huffman {:a 8 :b 3 :c 1 :d 1 :e 1 :f 1 :g 1 :h 1})

(defprotocol ComplexRepr
  (make-rectangular [this x y] [this])
  (make-polar [this r a] [this])
  (magnitude [this])
  (angle [this]))

(defrecord RectComplex [x y])

(defrecord PolarComplex [r a])

(extend-protocol ComplexRepr
  RectComplex
  (make-rectangular [this] this)
  (make-polar [this r a]
    (assoc this :r (* r (Math/cos a))
           :a (* r (Math/sin a))))
  (magnitude [this]
    (Math/sqrt (+ (Math/pow (:x this) 2)
                  (Math/pow (:y this) 2))))
  (angle [this] (Math/atan (/ (:y this) (:x this))))
  PolarComplex
  (make-rectangular [{:keys [r a] :as this}]
    (assoc this :x (Math/sqrt (+ (Math/pow r 2)
                                 (Math/pow a 2)))
           :y (Math/atan (/ a r))))
  (make-polar [this] this)
  (magnitude [this] (:r this))
  (angle [this] (:a this)))

(def rect-ex (->RectComplex 13 9))
(def polar-ex (->PolarComplex 13 9))

(make-rectangular polar-ex)
(rect-ex make-polar)

;; 2.45

(defmulti symbolic-diff (fn [exp var] [(class exp) (class var)]))
(defmethod symbolic-diff [java.lang.Long java.lang.Long] [_ _] 0)
(defmethod symbolic-diff [clojure.lang.Symbol clojure.lang.Symbol] [exp var]
  (if (= exp var) 1 0))

;; 2.47

(defmulti make-rect-multi class)
(defmethod make-rect-multi RectComplex [{:keys [x y]}]
  (fn [m] (condp = m
            :real x
            :imaginary y
            :magnitude (Math/sqrt (+ (Math/pow x 2) (Math/pow y 2)))
            :angle (Math/atan (/ y x)))))

((make-rect-multi rect-ex) :magnitude)

(defmulti make-polar-multi class)
(defmethod make-polar-multi PolarComplex [{:keys [r a]}]
  (fn [m] (condp = m
                 :magnitude r
                 :angle a)))

((make-polar-multi polar-ex) :magnitude)