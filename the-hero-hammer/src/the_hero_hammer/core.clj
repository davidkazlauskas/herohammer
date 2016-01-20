(ns the-hero-hammer.core
  (:require [compojure.core :refer :all]
            [the-hero-hammer.client_req_process :refer :all]
            [the-hero-hammer.questions_spec :refer :all]
            [org.httpkit.server :refer [run-server]])
  (:use hiccup.core))

(defn index
  "meow"
  []
  (html [:h1 "Dazlow!"]))

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

(defn lol-render-questions []
  (wrap-html [:form (map render-question (all-questions-lol))
              [:input {:type "submit" :value "submit"}]]))

(defroutes myapp
  (GET "/" [] (index))
  (GET "/questions" [] (lol-render-questions)))

(defn -main []
  (println "Muah runnin!")
  (run-server myapp {:port 5000}))
