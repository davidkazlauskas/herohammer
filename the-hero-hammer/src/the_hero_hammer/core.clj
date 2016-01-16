(ns the-hero-hammer.core
  (:require [compojure.core :refer :all]
            [org.httpkit.server :refer [run-server]])
  (:use hiccup.core))

(defn index []
  (html [:h1 "Dazlow!"]))

(defroutes myapp
  (GET "/" [] (index)))

(defn -main []
  (println "Muah runnin!")
  (run-server myapp {:port 5000}))
