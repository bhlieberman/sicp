(ns part-three
  (:require [clojure.repl :refer [doc source]]))

(def balance (atom 100))

(defn withdraw [amt]
  (if (> amt @balance)
    (throw (ex-info "Insufficient funds" {}))
    (- @balance amt)))

(defn new-withdraw []
  (let [balance 100]
    (fn [amt] (if (> amt balance)
                (throw (ex-info "Insufficient funds" {}))
                (- balance amt)))))

(defn make-withdraw [balance]
  (fn [amt]
    (if (> amt balance)
      (throw (ex-info "Insufficient funds" {}))
      (- balance amt))))

(def ^:dynamic *w-1* (make-withdraw 100))
(def w-2 (make-withdraw 100))

(do (binding [*w-1* (make-withdraw 150)]
      (println (*w-1* 100)))
    (println (*w-1* 100)))

(defmulti make-account (fn [{:keys [transaction]}] transaction))
(defmethod make-account :withdraw [{:keys [amount balance]}]
  (if (> amount balance)
    (throw (ex-info "Insufficient funds" {}))
    (- balance amount)))
(defmethod make-account :deposit [{:keys [amount balance]}]
  (+ balance amount))

(make-account {:transaction :deposit
               :amount 100
               :balance 50})

;; 3.1

(defn make-accumulator [n] (partial + n))

;; 3.2

(defn make-monitored [f]
  (fn [x] (let [x-count (atom 0)]
            (condp = x
              'how-many-calls? @x-count
              'reset-count (reset! x-count 0)
              (do (swap! x-count inc) (f x))))))

(((make-monitored make-accumulator) 300)) ; wth

;; 3.3

(defmulti make-account-w-password (fn [{:keys [transaction password]}]
                                    (if (some? password) transaction nil)))
(defmethod make-account-w-password nil [_] (throw (ex-info "you need a password" {})))
(defmethod make-account-w-password :withdraw [{:keys [amount balance]}]
  (if (> amount balance)
    (throw (ex-info "Insufficient funds" {}))
    (- balance amount)))
(defmethod make-account-w-password :deposit [{:keys [amount balance]}]
  (+ balance amount))
(defmethod make-account-w-password :create-acct [info] info)

(make-account-w-password {:transaction :deposit
                          :amount 100
                          :balance 50
                          :password "foobar"})

;; 3.4

(defmulti protected-acct (fn [{:keys [transaction password attempts]}]
                           (if (> @attempts 7)
                             :call-the-cops
                             (if (some? password) transaction nil))))
(defmethod protected-acct nil [_] "You need a password")
(defmethod protected-acct :withdraw [{:keys [amount balance]}]
  (if (> amount balance)
    (throw (ex-info "Insufficient funds" {}))
    (- balance amount)))
(defmethod protected-acct :deposit [{:keys [amount balance]}]
  (+ balance amount))
(defmethod protected-acct :call-the-cops [_]
  (println "Just kidding"))

(def transaction {:transaction :withdraw
                  :password nil
                  :attempts (atom 0)})

(dotimes [_ 8]
  (swap! (:attempts transaction) inc)
  (protected-acct transaction))

;; 3.5

(defn make-joint [acc old-pw new-pw]
  (if (some-> old-pw (= (:password acc)))
    (make-account-w-password (assoc acc :password new-pw))
    (make-account-w-password (assoc acc :password nil))))

#_(defmulti make-joint-acct
    (fn [acc old-pw new-pw]
      (-> (cond (= old-pw (:password acc))
                (->> new-pw
                     (assoc acc :password)))
          (assoc :transaction :create-acct))))

(def peter-acc (make-account-w-password {:transaction :create-acct
                                         :password "foobar"}))

(def paul-acc (make-joint peter-acc "foobar" nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; higher-order cons

(defn cons- [x y]
  (letfn [(dispatch [m]
            (cond
              (= m :car) x
              (= m :cdr) y
              :else
              (throw (UnsupportedOperationException. "Undefined operation -- CONS-"))))]
    dispatch))

(defn car [z] (z :carz))
(defn cdr [z] (z :cdr))

(car (cons- 1 3))
(cdr (cons- 17 4))

;; queues

(defprotocol IMutQueue
  (make-queue [this])
  (insert-queue! [this e])
  (delete-queue! [this e])
  (-empty? [this])
  (front-pointer [this])
  (rear-pointer [this])
  (set-front-ptr! [this e])
  (set-rear-ptr! [this e]))

(extend-protocol IMutQueue
  java.util.LinkedList
  (make-queue [_] (new java.util.LinkedList))
  (insert-queue! [this e] (.add this e))
  (delete-queue! [this _] (.remove this))
  (-empty? [this] (nil? (.peek this)))
  (front-pointer [this] (first this))
  (rear-pointer [this] (rest this))
  (set-front-ptr! [this e] (.offerFirst this e))
  (set-rear-ptr! [this e] (.offerLast this e)))

(def ll (new java.util.LinkedList))

(insert-queue! ll 3)
(-empty? ll)
(rear-pointer ll)