(ns the-hero-hammer.storage)

(def ^:dynamic *db-imitation* (java.util.concurrent.ConcurrentHashMap.))

(defn key-merge [arg]
  (cond (instance? String arg) arg
        (vector? arg) (interpose "-" arg)))

(defn get-key
  "Get data from storage with given key."
  [db-key]
  (.get *db-imitation* db-key))

(defn set-key
  "Put data into storage with specified key."
  [db-key value]
  (.put *db-imitation* db-key value))

(defn set-if-not-exists
  "Put data into storage if it doesn't exist"
  [db-key value]
  (if (nil? (get-key db-key))
    (set-key db-key value)))
