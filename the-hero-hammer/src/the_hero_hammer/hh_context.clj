(ns the-hero-hammer.hh_context
  (:require [taoensso.nippy :as nippy]))

(def ^:dynamic *hh-context* nil)

(defn get-key [& args]
  ((:get-key (:dbinfo *hh-context*)) args))
