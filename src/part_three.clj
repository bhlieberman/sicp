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

(make-account-w-password {:transaction :deposit
                          :amount 100
                          :balance 50
                          :password "foobar"})

;; 3.4

(defmulti protected-acct (fn [{:keys [transaction password attempts]}]
                           (if (> @attempts 7)
                             "Invoking procedure (call-the-cops)"
                             (if (some? password) transaction nil))))
(defmethod protected-acct nil [_] "You need a password")
(defmethod protected-acct :withdraw [{:keys [amount balance]}]
  (if (> amount balance)
    (throw (ex-info "Insufficient funds" {}))
    (- balance amount)))
(defmethod protected-acct :deposit [{:keys [amount balance]}]
  (+ balance amount))

(def transaction {:transaction :withdraw
                  :password nil
                  :attempts (atom 0)})

(dotimes [_ 8]
  (swap! (:attempts transaction) inc)
  (protected-acct transaction)) 

