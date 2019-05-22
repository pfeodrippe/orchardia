(ns orchard.info
  (:require [orchard.clr :as clr]
            [orchard.meta :as m]))

(defn info
  [ns sym]
  (or
   ;; it's a special (special-symbol?)
   (m/special-sym-meta sym)
   ;; it's a var
   (m/var-meta (m/resolve-var ns sym))
   ;; sym is an alias for another ns
   (m/ns-meta (get (m/resolve-aliases ns) sym))
   ;; it's simply a full ns
   (m/ns-meta (find-ns sym))
   ;; it's a Java class/member symbol...or nil
   (clr/resolve-symbol ns sym)))

(defn info-clr
  [class member]
  (clr/member-info class member))
