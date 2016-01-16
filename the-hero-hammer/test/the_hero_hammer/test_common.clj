(ns the-hero-hammer.test-common
  (:use [the-hero-hammer.routes :only [app]]
        [clojure.data.json :only [read-str]]))

(def test-app (app))

(defn read-json [str] (read-str str :key-fn keyword))
