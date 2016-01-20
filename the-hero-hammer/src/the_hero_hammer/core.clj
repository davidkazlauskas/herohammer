(ns the-hero-hammer.core
  (:require [compojure.core :refer :all]
            [the-hero-hammer.client_req_process :refer :all]
            [the-hero-hammer.questions_spec :refer :all]
            [org.httpkit.server :refer [run-server]])
  (:use hiccup.core
        [ring.middleware.params :only [wrap-params]]))

(defn index
  "meow"
  []
  (html [:h1 "Dazlow!"]))

(def ^:dynamic *radio-set*
  (into #{} (map #(str "radio-"
             (:shortname %1))
       (all-questions-lol))))

(defn render-question [q]
  (let [shortname (:shortname q)] (html
    [:p (:question q)]
   (map-indexed #(identity
         [:input {:type "radio" :value
                  (str "choice-val-" shortname "-" %1)
                  :name (str "radio-" shortname)
                  } %2])
        (:options q))
   [:br])))

(defn wrap-html [towrap]
  (html [:html
         [:head]
         [:body
          towrap]
         ]))

(defmacro q-post-link [] "/questions-post")

(defn lol-render-questions []
  (wrap-html [:form {:method "POST" :action (q-post-link)}
              (map render-question (all-questions-lol))
              [:input {:type "submit"
                       :value "Submit record"}]]))

(defn lol-post-questions [req]
  (println "par" (:params req))
  (html [:p (map #(html [:p %1]) req)]))

(defroutes myapp
  (GET "/" [] (index))
  (GET "/questions" [] (lol-render-questions))
  (POST (q-post-link) {params :params} (lol-post-questions params)))

(defn -main []
  (println "Muah runnin!")
  (run-server (wrap-params myapp) {:port 5000}))
