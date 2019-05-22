(ns compliment.sources.keywords
  "Completion for keywords interned globally across the application"
  (:require [clojure.string :as str]
            [compliment.sources :refer [defsource]]
            [compliment.utils :refer [defmemoized resolve-namespace]]))

(defmemoized ^:private keywords-table
  []
  (let [field (-> clojure.lang.Keyword
                  (.GetField (name '_symKeyMap)
                             (enum-or BindingFlags/Public
                                      BindingFlags/NonPublic
                                      BindingFlags/DeclaredOnly
                                      BindingFlags/Instance
                                      BindingFlags/Static))
                  (.GetValue nil))]
    (mapv #(let [target (.Target %)]
             (when target
               (identity [(subs (str target) 1)
                          (subs (str target) 1)])))
          (.Values field))))


(defn- tagged-candidate [c]
  {:candidate c, :type :keyword})

(defn qualified-candidates
  "Returns a list of namespace-qualified double-colon keywords (like ::foo)
  resolved for the given namespace."
  [prefix ns]
  (let [prefix (subs prefix 2)
        ns-name (str ns)]
    (for [[kw _] (keywords-table)
          :when (= (namespace kw) ns-name)
          :when (.startsWith (name kw) prefix)]
      (tagged-candidate (str "::" (name kw))))))

(defn namespace-alias-candidates
  "Returns a list of namespace aliases prefixed by double colon required in the
  given namespace."
  [prefix ns]
  (let [prefix (subs prefix 2)
        ns-name (str ns)]
    (for [[alias _] (ns-aliases ns)
          :let [aname (name alias)]
          :when (.startsWith aname prefix)]
      (tagged-candidate (str "::" aname)))))

(defn aliased-candidates
  "Returns a list of alias-qualified double-colon keywords (like ::str/foo),
  where alias has to be registered in the given namespace."
  [prefix ns]
  (when-let [[_ alias prefix] (re-matches #"::([^/]+)/(.*)" prefix)]
    (let [alias-ns-name (str (resolve-namespace (symbol alias) ns))]
      (for [[kw _] (keywords-table)
            :when (= (namespace kw) alias-ns-name)
            :when (str/starts-with? (name kw) prefix)]
        (tagged-candidate (str "::" alias "/" (name kw)))))))

(defn candidates
  [^String prefix, ns _]
  (let [single-colon? (str/starts-with? prefix ":")
        double-colon? (str/starts-with? prefix "::")
        has-slash? (> (.IndexOf prefix "/") -1)]
    (cond (and double-colon? has-slash?) (aliased-candidates prefix ns)
          double-colon? (concat (qualified-candidates prefix ns)
                                (namespace-alias-candidates prefix ns))
          single-colon? (for [[kw _] (keywords-table)
                              :when (str/starts-with? (str kw) (subs prefix 1))]
                          (tagged-candidate (str ":" kw))))))

(defsource ::keywords
  :candidates #'candidates
  :doc (constantly nil))
