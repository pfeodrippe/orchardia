(ns nrepl.middleware.info      ;; originally cider.nrepl.middleware.info
  (:require
   [clojure.string :as str]
   [orchard.eldoc :as eldoc]
   [orchard.info :as clj-info]
   [orchard.misc :as u]))

(defn info
  [{:keys [ns symbol class member] :as msg}]
  (let [[ns symbol class member] (map u/as-sym [ns symbol class member])]
    (let [var-info (cond (and ns symbol) (clj-info/info ns symbol)
                         (and class member) (clj-info/info-clr class member)
                         :else (throw (Exception.
                                       "Either \"symbol\", or (\"class\", \"member\") must be supplied")))
          ;; we have to use the resolved (real) namespace and name here
          see-also [] #_(clj-info/see-also (:ns var-info) (:name var-info))]
      (if (seq see-also)
        (merge {:see-also see-also} var-info)
        var-info))))

(defn eldoc-reply
  [msg]
  (if-let [info (info msg)]
    (eldoc/eldoc info)
    {:status :no-eldoc}))
