(ns orchard.clr
  (:require [clojure.string :as str]
            [orchard.introspection :as introspection]))

(def cache (atom {}))

(defn resolve-class
  "Given namespace and class symbols, search the imported classes and return
  class info. If not found, search all classes on the classpath (requires a
  qualified name)."
  [ns sym]
  (when-let [ns (find-ns ns)]
    (let [c (try (ns-resolve ns sym)
                 (catch Exception _))]
      c)))

(defn member-info
  [class member]
  (let [c (->> (introspection/methods-report class)
               (filter #(= (:name %) (str member))))]
    (when (seq c)
      {:class class
       :member member
       :arglists (map :parameters c)
       :return-type (map :return-type c)})))

(defn resolve-member
  "Given namespace and member symbols, search the imported classes and return
  a list of each matching member's info."
  [ns sym]
  (when-let [ns (find-ns ns)]
    (->> (vals (ns-imports ns))
         (map #(member-info % sym))
         (filter identity)
         (distinct))))

(defn resolve-symbol
  [ns sym]
  {:pre [(every? symbol? [ns sym])]}
  (let [name (-> (str sym)
                 (str/replace #"^\.|\.$" "")) ; strip leading/trailing dot
        sym* (symbol name)
        [class static-member] (->> (str/split name #"/" 2)
                                   (map #(when % (symbol %))))]
    (if-let [c (resolve-class ns class)]
      (if static-member
        (member-info c static-member)
        c)
      (when-let [ms (seq (resolve-member ns sym*))] ; methodCall
        (if (= 1 (count ms))
          (first ms)
          {:candidates (zipmap (map :class ms) ms)})))))
