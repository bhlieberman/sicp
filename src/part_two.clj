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
(defn pairs-non-neg [a b] (* (Math/pow 2 a) (Math/pow 3 b)))

(defn cons+ [a b] (fn [m] (m a b)))

(defn car [z] (z (fn [p _] p)))

(defn cdr [z] (z (fn [_ q] q)))

(cdr (cons+ 2 3))