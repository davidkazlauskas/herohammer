(ns the-hero-hammer.core
  (:require [compojure.core :refer :all]
            [org.httpkit.server :refer [run-server]]))

(defroutes myapp
  (GET "/" [] "Hello World"))

(defn -main []
  (run-server myapp {:port 5000}))
