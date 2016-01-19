(ns the-hero-hammer.storage)

(def ^:dynamic *db-imitation* (java.util.concurrent.ConcurrentHashMap.))

(defn get-key
  "Get data from storage with given key."
  [db-key]
  (.get *db-imitation* db-key))

(defn set-key
  "Put data into storage with specified key."
  [db-key value]
  (.put *db-imitation* db-key value))
