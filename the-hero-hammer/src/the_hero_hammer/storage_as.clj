(ns the-hero-hammer.storage_as
  (:import [com.aerospike.client AerospikeClient]
           [com.aerospike.client.policy WritePolicy]
           ))

(def ^:dynamic *db-imitation* (java.util.concurrent.ConcurrentHashMap.))

(defn make-aerospike-context [ip port]
  (let [cl (com.aerospike.client.AerospikeClient. ip port)
        wp (com.aerospike.client.policy.WritePolicy.)]
    {:get-key "" }
    ))

(defn key-merge [arg]
  (cond (instance? String arg) arg
        (or (vector? arg) (list? arg) (seq arg))
          (clojure.string/join "-" arg)))

(defn get-key
  "Get data from storage with given key."
  [db-key]
  (.get *db-imitation* (key-merge db-key)))

(defn set-key
  "Put data into storage with specified key."
  [db-key value]
  (.put *db-imitation* (key-merge db-key) value))

(defn set-if-not-exists
  "Put data into storage if it doesn't exist"
  [db-key value]
  (let [merged (key-merge db-key)]
    (if (nil? (get-key merged))
      (set-key merged value))))

(defn get-key-batch
  "Get keys in batch."
  [db-keys]
  (->> db-keys
       (map get-key)
       (into [])))

(def ^:dynamic *storage-ram-context*
  {:get-key get-key
   :set-key set-key
   :set-if-not-exists set-if-not-exists
   :get-key-batch get-key-batch})
