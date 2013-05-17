(ns classReader.core
  (:gen-class)
  (:require [clojure.java.io :as io])
  (:import [java.io DataInputStream]
           [java.nio ByteBuffer]))

(defn read-u4
  [stream]
  (.readInt stream))
(defn read-u2
  [stream]
  (.readUnsignedShort stream))
(defn read-u1
  [stream]
  (.readUnsignedByte stream))
(defn read-float
  [stream]
  (.readFloat stream))
(defn read-double
  [stream]
  (.readDouble stream))
(defn read-long
  [stream]
  (.readLong stream))

;;Constant pool entry tags
(def ctag-types
  {1 :utf8
   3 :integer
   4 :float
   5 :long
   6 :double
   7 :class
   8 :string
   9 :fieldref
   10 :methodref
   11 :interfacemethodref
   12 :nameandtype })

;;Access flags
(def acc-public 0x0001)
(def acc-private 0x0002)
(def acc-protected 0x0004)
(def acc-static 0x0008)
(def acc-final 0x0010)
(def acc-volatile 0x0040)
(def acc-transient 0x0080)
(def access-flags
  {0x0001 :public
   0x0002 :private
   0x0004 :protected
   0x0008 :static
   0x0010 :final
   0x0040 :volatile
   0x0080 :transient})

(defn- cpool-tag
  [tag & kvs]
  (merge {:tag tag :entry-type (ctag-types tag)}
         (apply hash-map kvs)))
  
(defmulti read-cpool-entry
  (fn [tag stream] (ctag-types tag)))
(defmethod read-cpool-entry
  :utf8
  [tag stream]
  (let* [num-bytes (read-u2 stream)
         bytes (doall (take num-bytes (repeatedly #(read-u1 stream))))
         text (apply str (map char bytes))]
    (cpool-tag tag :num-bytes num-bytes :bytes bytes :text text)
    ))
(defmethod read-cpool-entry
  :integer
  [tag stream]
  (cpool-tag tag :bytes (read-u4 stream))
  )
(defmethod read-cpool-entry
  :float
  [tag stream]
  (cpool-tag tag :value (read-float stream))
  )
(defmethod read-cpool-entry
  :long
  [tag stream]
    (cpool-tag tag :value (read-long stream))
    )
(defmethod read-cpool-entry
  :double
  [tag stream]
    (cpool-tag tag :value (read-double stream))
    )
(defmethod read-cpool-entry
  :class
  [tag stream]
  (cpool-tag tag :name-index (read-u2 stream))
  )
(defmethod read-cpool-entry
  :string
  [tag stream]
  (cpool-tag tag :string-index (read-u2 stream))
  )
(defmethod read-cpool-entry
  :fieldref
  [tag stream]
  (let [class-index (read-u2 stream)
        name-type-index (read-u2 stream)]
    (cpool-tag tag :class-index class-index :name-type-index name-type-index)
    ))
(defmethod read-cpool-entry
  :methodref
  [tag stream]
  (let [class-index (read-u2 stream)
        name-type-index (read-u2 stream)]
    (cpool-tag tag :class-index class-index :name-type-index name-type-index)
    ))
(defmethod read-cpool-entry
  :interfacemethodref
  [tag stream]
  (let [class-index (read-u2 stream)
        name-type-index (read-u2 stream)]
    (cpool-tag tag :class-index class-index :name-type-index name-type-index)
    ))
(defmethod read-cpool-entry
  :nameandtype
  [tag stream]
  (let [name-index (read-u2 stream)
        descriptor-index (read-u2 stream)]
    (cpool-tag tag :name-index name-index :descriptor-index descriptor-index)
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
  "interfaces are indexes into the constant pool"
  [stream]
  (read-u2 stream))
(defn read-interfaces
  [stream]
  (let [interface-count (read-u2 stream)]
    (println interface-count "interfaces")
    (doall
     (take interface-count
           (repeatedly
            (fn [] (read-interface-entry stream)))))))

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

(defn associate-names
  [constant-pool]
  (map (fn [entry]
         (if-let [name-idx (entry :name-index)]
           (assoc entry :name (:text (nth constant-pool (dec name-idx))))
           entry))
       constant-pool))

(defn read-class
  [filename]
  (let [class-stream (DataInputStream. (io/input-stream filename))]
    (let [magic (read-u4 class-stream)
          minor-version (read-u2 class-stream)
          major-version (read-u2 class-stream)
          constant-pool (associate-names
                         (read-constant-pool class-stream))
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
