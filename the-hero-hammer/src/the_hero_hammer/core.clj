(ns the-hero-hammer.core
  (:require [compojure.core :refer :all]
            [org.httpkit.server :refer [run-server]])
  (:use hiccup.core))

(defn index
  "meow"
  []
  (html [:h1 "Dazlow!"]))

(defn questions []
  (["Do you like, have a beard?" "yes" "nay"]))

(defn render-question [q]
  (html [:form
         [:p (nth q 0)]
         (map #(1) (drop 1 q))
         ]))

(defn compute-unit [func keyindb]
  {:processed 0 :count 0})

(render-question (first (questions)))

(defroutes myapp
  (GET "/" [] (index)))

(defn -main []
  (println "Muah runnin!")
  (run-server myapp {:port 5000}))
