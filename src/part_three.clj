(ns part-three)

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

