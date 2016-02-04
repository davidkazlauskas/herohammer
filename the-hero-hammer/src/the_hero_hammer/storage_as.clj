(ns the-hero-hammer.storage_as
  (:import [com.aerospike.client AerospikeClient Key Bin Operation]
           [com.aerospike.client.policy WritePolicy QueryPolicy BatchPolicy]))

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
        get-first (.get client policy key-aes)
        the-value (if get-first
                    (.getValue get-first "default"))]
    the-value))

(defn get-key-batch-aes [client policy the-keys]
  (let [keys-aes
         (into-array
           (mapv #(let [as-ns (nth % 0)
                        as-set (nth % 1)
                        as-idx (nth % 2)]
                   (Key. as-ns as-set as-idx)) the-keys))
        get-first (.get client policy keys-aes)
        the-value (mapv #(.getValue % "default") get-first)]
    the-value))

(defn record-exists-aes [client policy the-key]
  (let [as-ns (nth the-key 0)
        as-set (nth the-key 1)
        as-idx (nth the-key 2)
        key-aes (Key. as-ns as-set as-idx)
        the-val (.exists client policy key-aes)]
    the-val))

(defn atomic-increment-aes [client policy the-key]
  (let [as-ns (nth the-key 0)
        as-set (nth the-key 1)
        as-idx (nth the-key 2)
        add-op (Operation/add (Bin. "default" 1))
        get-op (Operation/get "default")
        key-aes (Key. as-ns as-set as-idx)
        the-val (.operate client policy key-aes
                          (into-array [add-op get-op]))]
    (dec (.getLong the-val "default"))))

(defn make-aerospike-context [ip port]
  (try
    (let [cl (AerospikeClient. ip port)
        wp (WritePolicy.)
        rp (QueryPolicy.)
        bp (BatchPolicy.)]
    {:get-key (partial get-key-aes cl rp)
     :set-key (partial set-key-aes cl wp)
     :get-key-batch (partial get-key-batch-aes cl bp)
     :atomic-increment (partial atomic-increment-aes cl wp)
     :exists (partial record-exists-aes cl rp)})
    (catch Exception e nil)))

(defn aes-serv []
  (or (System/getenv "AES_URL")
      "192.168.56.101"))

(defn aes-port []
  (or (Integer. (System/getenv "AES_PORT"))
      3000))

(def ^:dynamic *aes-client* (make-aerospike-context (aes-serv) (aes-port)))
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
  (let [client (*get-aes-client*)
        exists-func (:exists client)
        set-func (:set-key client)]
    (if (not (exists-func db-key))
             (set-func db-key value))))

(defn get-key-batch
  "Get keys in batch."
  [db-keys]
  (let [client (*get-aes-client*)
        func (:get-key-batch client)]
    (func db-keys)))

(defn atomic-increment
  "Atomic increment and return the result."
  [db-key]
  (let [client (*get-aes-client*)
        func (:atomic-increment client)]
    (func db-key)))

(def ^:dynamic *storage-aes-context*
  {:get-key get-key
   :set-key set-key
   :set-if-not-exists set-if-not-exists
   :get-key-batch get-key-batch
   :atomic-increment atomic-increment})
