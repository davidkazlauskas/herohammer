(ns the-hero-hammer.db_context)

; EZ START
;(def ^:dynamic *get-db-context* (fn [] the-hero-hammer.storage_ram/*storage-ram-context*))

(def ^:dynamic *db-context* nil)

(def ^:dynamic *get-db-context* nil)

(def ^:dynamic *allow-writes*
  (if (System/getenv "HH_ALLOW_WRITES")
    (do
      (println "WRITES ARE ALLOWED (HH_ALLOW_WRITES)")
      true)
    (do
      (println "WRITES ARE PREVENTED (HH_ALLOW_WRITES doesnt exist)")
      false)))

(defn fn-get-key []
  (:get-key (*get-db-context*)))

(defn fn-set-key []
  (:set-key (*get-db-context*)))

(defn fn-set-if-not-exists []
  (:set-if-not-exists (*get-db-context*)))

(defn fn-get-keys-batch []
  (:get-key-batch (*get-db-context*)))

(defn fn-atomic-increment []
  (:atomic-increment (*get-db-context*)))

(defn get-key
  "Get key"
  [db-key]
  ((fn-get-key) db-key))

(defn set-key
  "Set key"
  [db-key value]
  (if *allow-writes*
   ((fn-set-key) db-key value)
   (println "Would write (prevented): " db-key "->" value)))

(defn set-if-not-exists
  "Put data into storage if it doesn't exist"
  [db-key value]
  (if *allow-writes*
    ((fn-set-if-not-exists) db-key value)
    (println "Would write (prevented): " db-key "->" value)))

(defn get-key-batch
  "Get keys in batch."
  [db-keys]
  ((fn-get-keys-batch) db-keys))

(defn atomic-increment-key
  "Atomic increment on key."
  [db-key]
  (if *allow-writes*
    ((fn-atomic-increment) db-key)
    (println "Would increment" db-key)))
