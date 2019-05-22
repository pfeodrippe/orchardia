(ns compliment.sources.class-members
  (:require [clojure.string :as str]
            [compliment.sources :refer [defsource]]
            [compliment.sources.local-bindings :refer [bindings-from-context]]
            [compliment.utils :refer [fuzzy-matches-no-skip? resolve-class]]
            [orchard.introspection :as introspection])
  (:import [System.Reflection MonoMethod MonoProperty]))

(defn static?
  [member]
  (if (= MonoProperty (type member))
    (static? (.GetMethod member))
    (.IsStatic member)))

(def members-cache
  "Stores cache of all non-static members for every namespace."
  (atom {}))

(defn camel-case-matches?
  "Tests if prefix matches the member name following camel case rules.
  Thus, prefix `getDeF` matches member `getDeclaredFields`."
  [prefix member-name]
  (fuzzy-matches-no-skip? prefix member-name #(Char/IsUpper %)))

(defn populate-members-cache
  "Populate members cache of class members for `ns` from the given list of
  classes. `imported-classes-cnt` is a number that indicates the current number
  of imported classes in this namespace."
  [ns classes imported-classes-cnt]
  (let [members
        (doall
         (for [class classes
               member (->> (concat (introspection/methods class)
                                   (introspection/properties class)
                                   (if (isa? class System.Enum)
                                     (introspection/fields class)))
                           (take 5))
               :when (not (static? member))]
           (let [dc (.DeclaringType member)]
             (if (= dc class)
               member
               (if (instance? MonoMethod member)
                 (.GetMethod
                  dc
                  (.Name member)
                  (enum-or
                   BindingFlags/FlattenHierarchy
                   BindingFlags/Static
                   BindingFlags/Instance
                   BindingFlags/Public
                   BindingFlags/NonPublic)
                  nil
                  CallingConventions/Any
                  (->> (.GetParameters member)
                       (mapv #(.ParameterType %))
                       (into-array Type))
                  nil)
                 (.GetField dc (.Name member)))))))
        cache
        (reduce (fn [cache, m]
                  (let [full-name (.Name m)]
                    (assoc! cache full-name (conj (cache full-name []) m))))
                (transient {})
                (remove nil? members))]
    (swap! members-cache assoc ns {:classes (set classes)
                                   :imported-classes-cnt imported-classes-cnt
                                   :members (persistent! cache)})))

(defn update-cache
  "Updates members cache for a given namespace if necessary."
  ([ns] (update-cache ns nil))
  ([ns context-class]
   (let [imported-classes (set (filter class? (vals (ns-map ns))))
         imported-classes-cnt (count imported-classes)
         cache (@members-cache ns)]
     (println :IMM imported-classes-cnt)
     (when (or (nil? cache)
               (not= (:imported-classes-cnt cache) imported-classes-cnt)
               (and context-class
                    (not (contains? (:classes cache) context-class))))
       (let [classes (cond-> (into imported-classes (:classes cache))
                       context-class (conj context-class))]
         (populate-members-cache ns classes imported-classes-cnt))))))

(defn get-all-members
  "Returns all non-static members for a given namespace."
  [ns context-class]
  (update-cache ns context-class)
  (get-in @members-cache [ns :members]))

(defn class-member-symbol?
  "Tests if a symbol name looks like a non-static class member."
  [^String x]
  (str/starts-with? x "."))

(defn try-get-object-class
  "Tries to get the type of the object from the context, which the member will be
  applied to. Object should be a symbol resolving to a Var or have a type tag."
  [ns context]
  (when (= (:idx (first context)) 0)
    (let [form (second (:form (first context)))]
      (if-let [tag (or
                    ;; Form might have an immediate tag...
                    (:tag (meta form))
                    ;; ...or a tag somewhere in local scope. Note how getting
                    ;; an element from a set can return itself but with meta.
                    (:tag (meta (get (set (bindings-from-context context)) form))))]
        ;; We have a tag - try to resolve the class from it.
        (resolve-class ns tag)
        ;; Otherwise, try to resolve symbol to a Var.
        (let [obj (and (symbol? form) (ns-resolve ns form))]
          (and (= (class obj) clojure.lang.Var)
               (class (deref obj))))))))

(defn members-candidates
  "Returns a list of Java non-static fields and methods candidates."
  [prefix ns context]
  (when (class-member-symbol? prefix)
    (let [prefix' (subs prefix 2)
          inparts? (re-find #"[A-Z]" prefix')
          prefix (subs prefix 1)
          klass (try-get-object-class ns context)]
      (println :k klass)
      (for [[member-name members] (get-all-members ns klass)
            :when (if inparts?
                    (camel-case-matches? prefix member-name)
                    (str/starts-with? ^String member-name prefix))
            :when
            (do #_(clojure.pprint/pprint {:member-name member-name
                                        :prefix prefix
                                        :inpa inparts?
                                        :aaaaa (not klass)
                                        :bbbbb
                                        members
                                        :cccc
                                        (mapv #(.DeclaringType %)
                                              members)})
                (or (not klass)
                    (some #(= klass (.DeclaringType %)) members)))]
        {:candidate (str "." member-name)
         :type (if (instance? MonoMethod (first members))
                 :method :field)}))))

(defsource ::members
  :candidates #'members-candidates
  :doc (constantly nil))

;; Works only with static members for now
;; Static Methos

(def ^{:doc "Stores cache of all static members for every class."}
  static-members-cache (atom {}))

(defn populate-static-members-cache
  "Populates static members cache for a given class."
  [class]
  (loop [cache {}, [c & r] (concat (introspection/methods class)
                                   (introspection/properties class)
                                   (if (isa? class System.Enum)
                                     (introspection/fields class)))]
    (if c
      (if (static? c)
        (let [full-name (.Name c)]
          (if (cache (.Name c))
            (recur (update-in cache [full-name] conj c) r)
            (recur (assoc cache full-name [c]) r)))
        (recur cache r))
      (swap! static-members-cache assoc class cache))))

(defn update-static-cache
  "Updates static members cache for a given class if necessary."
  [class]
  (when-not (@static-members-cache class)
    (populate-static-members-cache class)))

(defn static-member-symbol?
  "Tests if prefix looks like a static member symbol."
  [x]
  (re-matches #"[^\/\:\.][^\:]*\/.*" x))

(defn static-members
  "Returns all static members for a given class."
  [class]
  (update-static-cache class)
  (@static-members-cache class))

(defn static-members-candidates
  "Returns a list of static member candidates."
  [^String prefix, ns context]
  (when (static-member-symbol? prefix)
    (let [[cl-name member-prefix] (str/split prefix #"/")
          cl (resolve-class ns (symbol cl-name))
          member-prefix (or member-prefix "")]
      (when cl
        (let [inparts? (re-find #"[A-Z]" member-prefix)]
          (for [[^String member-name members] (static-members cl)
                :when  (if inparts?
                         (camel-case-matches? member-prefix member-name)
                         (str/starts-with? member-name member-prefix))]
            {:candidate (str cl-name "/" member-name)
             :type (if (instance? MonoMethod (first members))
                     :static-method :static-field)}))))))

(defn resolve-static-member
  "Given a string representation of a static member returns Member object."
  [^String member-str ns]
  (let [[cl-name member-name] (str/split member-str #"/")
        cl (resolve-class ns (symbol cl-name))]
    (when cl
      (update-static-cache cl)
      (get-in @static-members-cache [cl member-name]))))

(defsource ::static-members-candidates
  :candidates #'static-members-candidates
  :doc (constantly nil))
