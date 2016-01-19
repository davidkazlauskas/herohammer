(ns the-hero-hammer.storage)

(def ^:dynamic *db-imitation* (java.util.concurrent.ConcurrentHashMap.))

(defn get-key [db-key]
  (.get *db-imitation* db-key))

(defn set-key [db-key value]
  (.put *db-imitation* db-key value))
