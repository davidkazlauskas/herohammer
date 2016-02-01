
(ns the-hero-hammer.storage)

(def ^:dynamic *db-context* nil)

(defn get-db-context []
  *db-context*)

(defn fn-get-key []
  (:get-key (get-db-context)))

(defn fn-set-key []
  (:set-key (get-db-context)))

(defn fn-set-if-not-exists []
  (:set-if-not-exists (get-db-context)))

(defn fn-get-keys-batch []
  (:get-key-batch (get-db-context)))

(defn get-key
  "Get key"
  [db-key]
  ((fn-get-key) db-key))

(defn set-key
  "Set key"
  [db-key value]
  ((fn-set-key) db-key value))

(defn set-if-not-exists
  "Put data into storage if it doesn't exist"
  [db-key value]
  ((fn-set-if-not-exists) db-key value))

(defn get-key-batch
  "Get keys in batch."
  [db-keys]
  ((fn-get-keys-batch) db-keys))
