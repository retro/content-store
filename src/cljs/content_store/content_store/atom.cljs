(ns content-store.content-store.atom
  (:require [content-store.content-store :refer [not-empty-validator make-is-unique-type-validator]]
            [forms.validator :refer [validator]]
            [content-store.content-store.atom.constraints :as constraints]
            [content-store.content-store.atom.types :as types]))

(defn applicable-type-validation [field constraint-type]
  [:applicable-constraint
   (fn [v]
     (if (nil? v)
       true
       (let [constraint (get constraints/constraints constraint-type)]
         (if constraint
           (contains? (:applicable-types constraint) (:extends-type field))
           false))))])

(def constraint-arguments-required-validation
  [:constraint-arguments
   (fn [v]
     (or (nil? v) (types/valid-boolean? v nil)))])

(def constraint-arguments-predefined-value-validation
  [:constraint-arguments
   (fn [v]
     (if (nil? v)
       true
       (and (vector? v) (pos? (count v)))))])

(defn make-predefined-values-list-validation [field]
  [:predefined-value-type
   (fn [v]
     (if (nil? v)
       false
       (case (:extends-type field)
         :string   (string? v)
         :integer  (types/valid-integer? v nil)
         :float    (types/valid-float? v nil)
         :date     (types/valid-datetime? v nil)
         :datetime (types/valid-datetime? v nil)
         :location (types/valid-location? v nil)
         false)))])

(def constraint-arguments-format-validation
  [:constraint-arguments
   (fn [v]
     (cond
       (nil? v) true
       (or (string? v) (keyword? v)) (contains? (set (keys constraints/format-regexps)) (keyword v))
       :else false))])

(defn size-min-max-validation [{:keys [min max]}]
  (let [min-max [min max]
        every-valid? (every? true? (map (fn [v] (or (nil? v) (types/valid-integer? v nil))) min-max))]
    (if (and every-valid? (not (every? nil? min-max)))
      (if (and min max)
        (apply < (map (:integer types/field-value-converters) min-max))
        true)
      false)))

(def constraint-arguments-size-validation
  [:constraint-arguments
   (fn [v]
     (cond
       (nil? v) true
       (map? v) (size-min-max-validation v)
       :else false))])

(defn range-min-max-validation [{:keys [min max]}]
  (let [min-max [min max]
        every-valid? (every? true? (map (fn [v] (or (nil? v) (types/valid-float? v nil))) min-max))]
    (if (and every-valid? (not (every? nil? min-max)))
      (if (and min max)
        (apply < (map (:float types/field-value-converters) min-max))
        true)
      false)))

(def constraint-arguments-range-validation
  [:constraint-arguments
   (fn [v]
     (cond
       (nil? v) true
       (map? v) (range-min-max-validation v)
       :else false))])

(defn validate [store field]
  (let [vs {:type                           [not-empty-validator (make-is-unique-type-validator store)]
            :extends-type                   [types/valid-atom-validator]
            :choices                        [(types/make-has-valid-choices-validator field)]
            :constraints.required           [constraint-arguments-required-validation]
            :constraints.predefined-value   [(applicable-type-validation field :predefined-value)
                                             constraint-arguments-predefined-value-validation]
            :constraints.format             [(applicable-type-validation field :format)
                                             constraint-arguments-format-validation]
            :constraints.size               [(applicable-type-validation field :size)
                                             constraint-arguments-size-validation]
            :constraints.range              [(applicable-type-validation field :range)
                                             constraint-arguments-range-validation]}
        v  (validator vs)
        errors (v field)
        predefined-values (get-in field [:constraints :predefined-value])]
    (if (vector? predefined-values)
      (let [predefined-values-vs {:* [(make-predefined-values-list-validation field)]}
            predefined-values-v (validator predefined-values-vs)
            predefined-values-errors (predefined-values-v predefined-values)]
        (if (empty? predefined-values-errors)
          errors
          (assoc-in errors [:constraints :predefined-value] predefined-values-errors)))
      errors)))

(defn extract-deps [_] #{})

(defn nest-errors [errors]
  (if errors
    {:$errors$ errors}
    nil))

(defn make-setter [field]
  (fn [data]
    (let [valid-type? (types/validate-value-type field data)]
      (if valid-type?
        {:data (types/convert-value field data)
         :errors (nest-errors (constraints/validate-field-constraints field data))}
        {:data data
         :errors (nest-errors {:value data :failed [(keyword (str (name (:extends-type field)) "-type"))]})}))))
