(ns the-hero-hammer.storage_as
  (:import [com.aerospike.client AerospikeClient Key Bin]
           [com.aerospike.client.policy WritePolicy QueryPolicy]))

(def ^:dynamic *db-imitation* (java.util.concurrent.ConcurrentHashMap.))

(defn set-key-aes [client policy the-key the-value]
  (let [as-ns (nth the-key 0)
        as-set (nth the-key 1)
        as-idx (nth the-key 2)
        key-aes (Key. as-ns as-set as-idx)
        bin-aes (Bin. "default" the-value)
        ] (.put client policy key-aes (into-array [bin-aes]))))

(defn get-key-aes [client policy the-key]
  (let [as-ns (nth the-key 0)
        as-set (nth the-key 1)
        as-idx (nth the-key 2)
        key-aes (Key. as-ns as-set as-idx)
        get-first (.get client policy key-aes)]
    get-first))

(defn make-aerospike-context [ip port]
  (let [cl (AerospikeClient. ip port)
        wp (WritePolicy.)
        rp (QueryPolicy.)]
    {:get-key (partial get-key-aes cl rp)
     :set-key (partial set-key-aes cl wp)}
    ))

(def ^:dynamic *aes-client* (make-aerospike-context "192.168.56.101" 3000))
(def ^:dynamic *get-aes-client* (fn [] *aes-client*))

(defn key-merge [arg]
  (cond (instance? String arg) arg
        (or (vector? arg) (list? arg) (seq arg))
          (clojure.string/join "-" arg)))

(defn get-key
  "Get data from storage with given key."
  [db-key]
  (let [client (*get-aes-client*)
        func (:get-key client)]
  (println "dizzle" func)
   (func db-key)))

(defn set-key
  "Put data into storage with specified key."
  [db-key value]
  (let [client (*get-aes-client*)
        func (:set-key client)]
    (func db-key value)))

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
