(ns part-two)

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