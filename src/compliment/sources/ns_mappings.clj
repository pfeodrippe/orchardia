(ns compliment.sources.ns-mappings
  (:require [clojure.string :as str]
            [compliment.sources :refer [defsource]]
            [compliment.utils :refer [fuzzy-matches? resolve-namespace]]))

(defn var-symbol?
  "Test if prefix resembles a var name."
  [x]
  (re-matches #"([^\/\:][^\.\/]*([^\/\:]*\/[^\.\/]*)?)?" x))

(defn get-scope-and-prefix
  [s ns]
  (let [[scope-name sym] (if (> (.IndexOf s "/") -1)
                           (str/split s #"/") ())
        scope (when scope-name
                (resolve-namespace (symbol scope-name) ns))
        prefix (if scope
                 (or sym "") s)]
    [scope-name scope prefix]))

(defn dash-matches?
  "Tests if prefix partially matches a var name with dashes as
  separators."
  [prefix var]
  (fuzzy-matches? prefix var \-))

(defn candidates
  [prefix ns context]
  (when (var-symbol? prefix)
    (let [[scope-name scope prefix] (get-scope-and-prefix prefix ns)
          #_ #_ns-form-namespace (try-get-ns-from-context context)
          vars (cond
                 scope (ns-publics scope)
                 #_ #_ns-form-namespace (ns-publics ns-form-namespace)
                 :else (ns-map ns))]
      (count vars)
      (for [[var-sym var] vars
            :let [var-name (name var-sym)
                  {:keys [arglists doc] :as var-meta} (meta var)]
            :when (dash-matches? prefix var-name)]
        (if (= (type var) System.RuntimeType)
          {:candidate var-name, :type :class}
          (cond-> {:candidate (if scope
                                (str scope-name "/" var-name)
                                var-name)
                   :type (cond (:macro var-meta) :macro
                               arglists :function
                               :else :var)
                   :ns (str (or (:ns var-meta) ns))}
            arglists
            (assoc :arglists (apply list (map pr-str arglists)))

            #_doc
            #_(assoc :doc (generate-docstring var-meta))))))))

(defsource ::ns-mappings
  :candidates #'candidates
  :doc (constantly nil))
