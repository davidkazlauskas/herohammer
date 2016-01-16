(ns the-hero-hammer.handler
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
            [org.httpkit.server :as hkit])
  (:use [compojure.handler :only [site]]))

(defroutes app-routes
  (GET "/" [] "Hello World")
  (route/not-found "Not Found"))

(def app
  (hkit/run-server (site #'app-routes) {:port 8080}))
