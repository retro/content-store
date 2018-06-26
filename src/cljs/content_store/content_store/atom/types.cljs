(ns content-store.content-store.atom.types
  (:require [cljsjs.moment]
            [clojure.set :as set]
            [clojure.string :as str]))

(def valid-atom-types
  #{:string :date :datetime :integer :float :boolean :location})

(def valid-atom-validator
  [:valid-atom (fn [v _ _]
                 (contains? valid-atom-types v))])

(defn make-has-valid-choices-validator [field]
  [:valid-choices
   (fn [choices]
     (let [extends-type (:extends-type field)]
       (if (or (= :choice extends-type) (= :multiple-choice extends-type))
         (if (and (seqable? choices) (pos? (count choices)))
           (every? #(= 2 (count %)) (vec choices))
           false)
         true)))])

(def iso8601-regexp
  (js/RegExp. "^([\\+-]?\\d{4}(?!\\d{2}\\b))((-?)((0[1-9]|1[0-2])(\\3([12]\\d|0[1-9]|3[01]))?|W([0-4]\\d|5[0-2])(-?[1-7])?|(00[1-9]|0[1-9]\\d|[12]\\d{2}|3([0-5]\\d|6[1-6])))([T\\s]((([01]\\d|2[0-3])((:?)[0-5]\\d)?|24\\:?00)([\\.,]\\d+(?!:))?)?(\\17[0-5]\\d([\\.,]\\d+)?)?([zZ]|([\\+-])([01]\\d|2[0-3]):?([0-5]\\d)?)?)?)?$"))

(defn to-float [v]
  (if (string? v)
    (js/parseFloat v)
    v))

(defn valid-datetime? [v _]
  (let [moment-can-parse? (or (and (string? v) (.test iso8601-regexp v))
                              (instance? js/Date v))]
    (if moment-can-parse?
      (.isValid (js/moment v))
      false)))

(defn valid-integer? [v _]
  (if (string? v)
    (valid-integer? (js/parseFloat v))
    (if (number? v)
      (and (not (js/isNaN v))
           (= (.floor js/Math v) v))
      false)))

(defn valid-float? [v _]
  (if (string? v)
    (valid-float? (js/parseFloat v))
    (and (number? v) (not (js/isNaN v)))))

(def valid-booleans
  {:truthy #{"true" true "1" 1 "t"}
   :falsy  #{"false" false "0" 0 "f"}})

(defn valid-boolean? [v _]
  (let [booleans (apply set/union (vals valid-booleans))
        val (if (string? v) (str/lower-case v) v)]
    (contains? booleans val)))

(defn valid-location? [{:keys [longitude latitude]} _]
  (if (and (valid-float? longitude nil)
           (valid-float? latitude nil))
    (and (<= -90 (to-float latitude) 90)
         (<= -180 (to-float longitude) 180))
    false))

(defn valid-string? [v]
  (or (string? v) (nil? v)))

(defn integer-value-converter [v]
  (if (string? v)
    (js/parseInt v 10)
    v))

(defn cap-value [value lower-bound higher-bound]
  (cond
    (> value higher-bound) (rem value higher-bound)
    (< value lower-bound) (rem value lower-bound)
    :else value))

(def type-validations
  {:string   valid-string?
   :date     valid-datetime?
   :datetime valid-datetime?
   :integer  valid-integer?
   :float    valid-float?
   :boolean  valid-boolean?
   :location valid-location?})

(def field-value-converters
  {:string   (fn [v]
               (or v ""))
   :date     (fn [v]
               (.. (js/moment v)
                   (startOf "day")))
   :datetime (fn [v]
               (js/moment v))
   :integer  integer-value-converter
   :float    to-float
   :boolean  (fn [v]
               (let [val (if (string? v) (str/lower-case v) v)]
                 (contains? (:truthy valid-booleans) val)))
   :location (fn [{:keys [longitude latitude]}]
               {:longitude (cap-value (to-float longitude) -180 180)
                :latitude  (cap-value (to-float latitude) -90 90)})})

(defn validate-value-type [field value]
  (let [extends-type (:extends-type field)
        validator (get type-validations extends-type)]
    (validator value field)))

(defn convert-value [field value]
  (let [extends-type (:extends-type field)
        converter (or (get field-value-converters extends-type) identity)]
    (converter value)))
