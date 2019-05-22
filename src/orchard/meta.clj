(ns orchard.meta
  "Utility functions for extracting and manipulating metadata."
  (:require
   [clojure.repl :as repl]
   [clojure.string :as str]
   [orchard.spec :as spec]))

(def special-sub-symbs '{& fn*, catch try, finally try})

(defn- maybe-add-url
  "If `info` has a non blank :url or it's a :special-form build a :url
  entry pointing to https://clojure.org/..."
  [info]
  (if-let [url (cond
                 (not (str/blank? (:url info)))
                 (str "https://clojure.org/" (:url info))

                 (:special-form info)
                 (str "https://clojure.org/special_forms#" (:name info)))]
    (assoc info :url url)
    info))

(defn- maybe-add-file
  "If `meta-map` has no :file, assoc the canonical namespace source file."
  [{:keys [file ns] :as meta-map}]
  ;; If we don't know its file, use the ns file.
  (if (and ns (or (not file)
                  (re-find #"/form-init[^/]*$" file)))
    (-> (dissoc meta-map :line)
        #_(assoc :file (some-> (ns/canonical-source ns) .getPath)))
    meta-map))

(defn- maybe-protocol
  [info]
  (if-let [prot-meta (meta (:protocol info))]
    (merge info {:file (:file prot-meta)
                 :line (:line prot-meta)})
    info))

(declare var-name)

(defn- maybe-add-spec
  "If the var `v` has a spec has associated with it, assoc that into meta-map.
  The spec is formatted to avoid processing in the client (e.g. CIDER)."
  [v meta-map]
  (if-let [spec (when v (spec/spec-form (var-name v)))]
    (merge meta-map {:spec spec})
    meta-map))

(defn- map-seq [x]
  (if (seq x)
    x
    nil))

(defn resolve-var
  [ns sym]
  (if-let [ns (find-ns ns)]
    (try (ns-resolve ns sym)
         (catch Exception _
           nil))))

(defn resolve-aliases
  [ns]
  (if-let [ns (find-ns ns)]
    (ns-aliases ns)))

(def var-meta-whitelist
  [:ns :name :doc :file :arglists :forms :macro :special-form
   :protocol :line :column :static :added :deprecated :resource])

(defn var-meta
  "Return a map of metadata for var v.
  If whitelist is missing use var-meta-whitelist."
  ([v] (var-meta v var-meta-whitelist))
  ([v whitelist]
   (when (var? v)
     (let [meta-map (-> (meta v)
                        maybe-protocol
                        (select-keys (or whitelist var-meta-whitelist))
                        map-seq maybe-add-file maybe-add-url)]
       (maybe-add-spec v meta-map)))))

(defn ns-meta
  [ns]
  (when ns
    (merge
     (meta ns)
     {:ns ns
      :file (-> (ns-publics ns)
                first
                second
                var-meta
                :file)
      :line 1})))

(defn special-sym-meta
  "Return info for the symbol if it's a special-symbol?, or nil otherwise."
  [sym]
  (let [orig-sym sym
        sym (get special-sub-symbs sym sym)
        compiler-special? (special-symbol? orig-sym)]
    (when-let [m (and compiler-special? (#'repl/special-doc sym))]
      (-> m
          (assoc :name orig-sym)
          maybe-add-url))))

(defn meta+
  "Return special form or var's meta."
  [v]
  (or (special-sym-meta v)
      (meta v)))

(defn var-name
  "Return special form or var's namespace-qualified name as string."
  [v]
  (let [mta (meta+ v)]
    (if-let [ns (:ns mta)]
      (str (ns-name ns) "/" (:name mta))
      (name (:name mta)))))
