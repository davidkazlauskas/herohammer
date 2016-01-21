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

(defn form-to-data [form]
  (println "ferm " form)
  (into [] (map #(
     (println "|" (get % 0) "|->|" (get % 1) "|")
     (let [rawname (clojure.string/replace (get %1 0) "radio-" "")]
     (println "rew" rawname)
     [rawname
       (clojure.string/replace (get % 1)
         (str "choice-val-" rawname "-") "")]
     ["milky" "duck"]
     )
     ) form)))

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
  (wrap-html [:form {:id "questions-form"
                     :method "POST" :action (q-post-link)}
              [:select {:name "user-hero"}
               (map-indexed #(html
                   [:option {:value %1} %2]
                   ) (all-heroes-lol))
               ]
              [:select {:name "opponent-hero"}
               (map-indexed #(html
                   [:option {:value %1} %2]
                   ) (all-heroes-lol))
               ]
              (map render-question (all-questions-lol))
              [:textarea {:name "user-comment"
                          :rows 4 :cols 50}]
              [:input {:type "submit"
                       :value "Submit record"}]]))

(defn lol-question-set-similarity
  "Return percentage of values picked from user"
  [request]
  (let [cross (clojure.set/intersection *radio-set*
                (into #{} (keys request)))]
    (* (/ (count cross) (count *radio-set*)) 100)))

(defmacro min-questions [] 77)

(defn lol-post-questions [req]
  (let [answered (lol-question-set-similarity req)]
    (println "ans" answered)
    (if (> (min-questions) answered)
      (do
        (println req)
      (str "Only " answered "% of questions were answered."
           " Minimum is " (min-questions) "%."))
      (html [:p (map #(html [:p %1]) req)])
      )
    )
  )

(defroutes myapp
  (GET "/" [] (index))
  (GET "/questions" [] (lol-render-questions))
  (POST (q-post-link) {params :params} (lol-post-questions params)))

(defn -main []
  (println "Muah runnin!")
  (run-server (wrap-params myapp) {:port 5000}))
