(ns compliment.core
  "Core namespace. Most interactions with Compliment should happen
  through functions defined here."
  (:require (compliment.sources ns-mappings
                                namespaces-and-classes
                                class-members
                                keywords
                                special-forms
                                #_local-bindings
                                #_resources)
            [compliment.sources :refer [all-sources]]
            [compliment.context :refer [cache-context]]
            [compliment.utils]
            [clojure.string :refer [join]]))

(defn ensure-ns
  "Takes either a namespace object or a symbol and returns the corresponding
  namespace if it exists, otherwise returns `user` namespace."
  [nspc]
  (cond (instance? clojure.lang.Namespace nspc) nspc
        (symbol? nspc) (or (find-ns nspc) (find-ns 'user) *ns*)
        :else *ns*))

(defn completions
  "Returns a list of completions for the given prefix. Options map can contain
  the following options:
  - :ns - namespace where completion is initiated;
  - :context - code form around the prefix;
  - :sort-order (either :by-length or :by-name);
  - :plain-candidates - if true, returns plain strings instead of maps;
  - :extra-metadata - set of extra fields to add to the maps;
  - :sources - list of source keywords to use."
  ([prefix]
   (completions prefix {}))
  ([prefix options-map]
   (if (string? options-map)
     (completions prefix {:context options-map})
     (let [{:keys [context sources]}
           options-map

           nspc (ensure-ns (:ns options-map))
           options-map (assoc options-map :ns nspc)
           ctx (cache-context context)]
       (let [candidate-fns (keep (fn [[_ src]]
                                   (when (:enabled src)
                                     (:candidates src)))
                                 (if sources
                                   (all-sources sources)
                                   (all-sources)))
             candidates (mapcat
                         (fn [f] (f prefix nspc ctx))
                         candidate-fns)
             sorted-cands (sort-by
                           :candidate
                           candidates)
             cands (if (:plain-candidates options-map)
                     (map :candidate sorted-cands)
                     sorted-cands)]
         (doall cands))))))

#_(defn documentation
  "Returns a documentation string that describes the given symbol."
  ([symbol-str]
   (documentation symbol-str *ns*))
  ([symbol-str ns]
   (if (empty? symbol-str)
     ""
     (->> (for [[_ {:keys [doc enabled]}] (all-sources)
                :when enabled
                :let [docstr (doc symbol-str (ensure-ns ns))]
                :when docstr]
            docstr)
          (interpose "\n\n")
          join))))
