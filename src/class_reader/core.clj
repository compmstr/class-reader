(ns class-reader.core
  (:require [clojure.java.io :as io])
  (:import [java.io DataInputStream]))

(defn read-u4
  [stream]
  (.readInt stream))
(defn read-u2
  [stream]
  (.readUnsignedShort stream))
(defn read-u1
  [stream]
  (.readUnsignedByte stream))

;;Constant pool entry tags
(def ctag-utf8 1)
(def ctag-integer 3)
(def ctag-float 4)
(def ctag-long 5)
(def ctag-double 6)
(def ctag-class 7)
(def ctag-string 8)
(def ctag-fieldref 9)
(def ctag-methodref 10)
(def ctag-interfacemethodref 11)
(def ctag-nameandtype 12)

(defmulti read-cpool-entry
  (fn [tag stream] tag))
(defmethod read-cpool-entry
  ctag-utf8
  [tag stream]
  (let* [num-bytes (read-u2 stream)
         bytes (doall (take num-bytes (repeatedly #(read-u1 stream))))
         text (apply str (map char bytes))]
    {:tag tag :num-bytes num-bytes :bytes bytes :text text}
    ))
(defmethod read-cpool-entry
  ctag-integer
  [tag stream]
  {:tag tag :bytes (read-u4 stream)}
  )
(defmethod read-cpool-entry
  ctag-float
  [tag stream]
  {:tag tag :bytes (read-u4 stream)}
  )
(defmethod read-cpool-entry
  ctag-long
  [tag stream]
  (let [high-bytes (read-u4 stream)
        low-bytes (read-u4 stream)]
    {:tag tag :high-bytes high-bytes :low-bytes low-bytes}
    ))
(defmethod read-cpool-entry
  ctag-double
  [tag stream]
  (let [high-bytes (read-u4 stream)
        low-bytes (read-u4 stream)]
    {:tag tag :high-bytes high-bytes :low-bytes low-bytes}
    ))
(defmethod read-cpool-entry
  ctag-class
  [tag stream]
  {:tag tag :name-idx (read-u2 stream)}
  )
(defmethod read-cpool-entry
  ctag-string
  [tag stream]
  {:tag tag :string-idx (read-u2 stream)}
  )
(defmethod read-cpool-entry
  ctag-fieldref
  [tag stream]
  (let [class-idx (read-u2 stream)
        name-type-idx (read-u2 stream)]
    {:tag tag :class-idx class-idx :name-type-idx name-type-idx}
    ))
(defmethod read-cpool-entry
  ctag-methodref
  [tag stream]
  (let [class-idx (read-u2 stream)
        name-type-idx (read-u2 stream)]
    {:tag tag :class-idx class-idx :name-type-idx name-type-idx}
    ))
(defmethod read-cpool-entry
  ctag-interfacemethodref
  [tag stream]
  (let [class-idx (read-u2 stream)
        name-type-idx (read-u2 stream)]
    {:tag tag :class-idx class-idx :name-type-idx name-type-idx}
    ))
(defmethod read-cpool-entry
  ctag-nameandtype
  [tag stream]
  (let [name-idx (read-u2 stream)
        descriptor-idx (read-u2 stream)]
    {:tag tag :name-idx name-idx :descriptor-idx descriptor-idx}
    ))
(defn read-constant-pool
  [stream]
  (let [cpool-count (dec (read-u2 stream))]
    (println cpool-count " constant pool entries")
    (doall 
    (map (fn [_]
           (let [cpool-tag (read-u1 stream)]
             (println "Reading cpool entry with tag: " cpool-tag)
             (read-cpool-entry cpool-tag stream)))
         (range cpool-count))
    )))

(defn read-interface-entry
  [stream])
(defn read-interfaces
  [stream])

(defn read-field-entry
  [stream])
(defn read-fields
  [stream])

(defn read-method-entry
  [stream])
(defn read-methods
  [stream])

(defn read-attribute-entry
  [stream])
(defn read-attributes
  [stream])

(defn read-class
  [filename]
  (let [class-stream (DataInputStream. (io/input-stream filename))]
    (let [magic (read-u4 class-stream)
          minor-version (read-u2 class-stream)
          major-version (read-u2 class-stream)
          constant-pool (read-constant-pool class-stream)
          access-flags (read-u2 class-stream)
          this-class (read-u2 class-stream)
          super-class (read-u2 class-stream)
          interfaces (read-interfaces class-stream)
          methods (read-methods class-stream)
          attributes (read-attributes class-stream)]
      {:magic-num magic
       :minor-version minor-version
       :major-version major-version
       :constant-pool constant-pool
       :access-flags access-flags
       :this-class this-class
       :super-class super-class
       :interfaces interfaces
       :methods methods
       :attributes attributes}
      )))
