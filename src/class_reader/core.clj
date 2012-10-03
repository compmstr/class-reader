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

;;Access flags
(def acc-public 0x0001)
(def acc-private 0x0002)
(def acc-protected 0x0004)
(def acc-static 0x0008)
(def acc-final 0x0010)
(def acc-volatile 0x0040)
(def acc-transient 0x0080)

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
  {:tag tag :name-index (read-u2 stream)}
  )
(defmethod read-cpool-entry
  ctag-string
  [tag stream]
  {:tag tag :string-index (read-u2 stream)}
  )
(defmethod read-cpool-entry
  ctag-fieldref
  [tag stream]
  (let [class-index (read-u2 stream)
        name-type-index (read-u2 stream)]
    {:tag tag :class-index class-index :name-type-index name-type-index}
    ))
(defmethod read-cpool-entry
  ctag-methodref
  [tag stream]
  (let [class-index (read-u2 stream)
        name-type-index (read-u2 stream)]
    {:tag tag :class-index class-index :name-type-index name-type-index}
    ))
(defmethod read-cpool-entry
  ctag-interfacemethodref
  [tag stream]
  (let [class-index (read-u2 stream)
        name-type-index (read-u2 stream)]
    {:tag tag :class-index class-index :name-type-index name-type-index}
    ))
(defmethod read-cpool-entry
  ctag-nameandtype
  [tag stream]
  (let [name-index (read-u2 stream)
        descriptor-index (read-u2 stream)]
    {:tag tag :name-index name-index :descriptor-index descriptor-index}
    ))
(defn read-constant-pool
  [stream]
  (let [cpool-count (dec (read-u2 stream))]
    (println cpool-count " constant pool entries")
    (doall 
     (take cpool-count (repeatedly (fn []
            (let [cpool-tag (read-u1 stream)]
              (read-cpool-entry cpool-tag stream)))
          )))))

(defn read-interface-entry
  [stream]
  {:tag ctag-class :name-index (read-u2 stream)})
(defn read-interfaces
  [stream]
  (let [interface-count (read-u2 stream)]
    (println interface-count "interfaces")
    (doall
     (take interface-count (repeatedly (fn []
            (let [interface-tag (read-u1 stream)]
              (if (not (= interface-tag ctag-class))
                (throw (Exception. "Invalid interface read"))
                (read-interface-entry stream)))))))))

(defn read-attribute-entry
  [stream]
  (let [name-index (read-u2 stream)
        attribute-length (read-u4 stream)
        attribute-info (doall (take attribute-length
                             (repeatedly #(read-u1 stream))))]
    {:name-index name-index
     :attribute-length attribute-length
     :attribute-info attribute-info}))
(defn read-attributes
  [stream]
  (let [attribute-count (read-u2 stream)]
    (doall (take attribute-count (repeatedly
                           (fn []
                             (read-attribute-entry stream)))))))

(defn read-field-entry
  [stream]
  (let [access-flags (read-u2 stream)
        name-index (read-u2 stream)
        descriptor-index (read-u2 stream)
        attributes (read-attributes stream)]
    {:access-flags access-flags
     :name-index name-index
     :descriptor-index descriptor-index
     :attributes attributes}
  ))
(defn read-fields
  [stream]
  (let [field-count (read-u2 stream)]
    (println field-count "fields")
    (doall
     (take field-count (repeatedly
                        (fn [] (read-field-entry stream)))))))

(defn read-method-entry
  [stream]
  (let [access-flags (read-u2 stream)
        name-index (read-u2 stream)
        descriptor-index (read-u2 stream)
        attributes (read-attributes stream)]
    {:access-flags access-flags
     :name-index name-index
     :descriptor-index descriptor-index
     :attributes attributes}
  ))
(defn read-methods
  [stream]
  (let [method-count (read-u2 stream)]
    (println method-count "methods")
    (doall
     (take method-count (repeatedly
                        (fn [] (read-method-entry stream)))))))

(defn get-class-constant
  [class index]
  (nth (:constant-pool class) (dec index)))

(defn super-class-name
  [class]
  (let* [class-entry (get-class-constant class (:super-class class))
         class-name (get-class-constant class (:name-index class-entry))]
        (:text class-name)))

(defn class-name
  [class]
  (let* [class-entry (get-class-constant class (:this-class class))
         class-name (get-class-constant class (:name-index class-entry))]
        (:text class-name)))

(defn field-name
  [class field]
  (:text (get-class-constant class (:name-index field))))

(defn field-names
  [class]
  (map #(field-name class %) (:fields class)))

(defn method-name
  [class method]
  (:text (get-class-constant class (:name-index method))))

(defn method-names
  [class]
  (map #(method-name class %) (:methods class)))

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
          fields (read-fields class-stream)
          methods (read-methods class-stream)
          attributes (read-attributes class-stream)]
      (.close class-stream)
      {:magic-num magic
       :minor-version minor-version
       :major-version major-version
       :constant-pool constant-pool
       :access-flags access-flags
       :this-class this-class
       :super-class super-class
       :interfaces interfaces
       :fields fields
       :methods methods
       :attributes attributes}
      )))
