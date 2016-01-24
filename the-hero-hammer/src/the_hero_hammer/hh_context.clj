(ns the-hero-hammer.hh_context)

(def ^:dynamic *hh-context* nil)

(defn get-key [& args]
  (apply (:get-key (:dbinfo *hh-context*)) args))

(defn set-key [& args]
  (apply (:set-key (:dbinfo *hh-context*)) args))

(defn set-if-not-exists
  "Put data into storage if it doesn't exist"
  [db-key value]
  (if (nil? (get-key db-key))
    (set-key db-key value)))
