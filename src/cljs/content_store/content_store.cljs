(ns content-store.content-store
  (:require [forms.validator :refer [validator]]))

(defn type-unique-in-store? [store type]
  (= 1 (count (filter #(= type %) (map :type store)))))

(defn make-is-unique-type-validator [store]
  [:is-unique (fn [v] (type-unique-in-store? store v))])

(defn zero-count? [v]
  (if (satisfies? ICounted v)
    (zero? (count v))
    false))

(defn not-empty-validator-fn [v _ _]
  (cond
    (nil? v) false
    (= "" v) false
    (zero-count? v) false
    :else true))

(def not-empty-validator
  [:not-empty not-empty-validator-fn])

(def allowed-field-deps
  {:block #{:atom :block :collection :compound}
   :collection #{:atom :block :compound}
   :compound #{:atom :block :collection}
   :content-type #{:atom :block :collection :compound}})

(defn make-valid-field-deps-validator [store type]
  (let [store-field-types (reduce (fn [acc v] (assoc acc (:type v) (:field-type v))) {} store)]
    [:valid-fields (fn [v]
                     (let [field-type (get store-field-types v)]
                       (contains? (get allowed-field-deps type) field-type)))]))

(defn make-size-validation [{:keys [min max]}]
  (let [min (or min 0)
        max (or max js/Infinity)]
    (fn [v _ _]
      (and (satisfies? ICounted v) (<= min (count v) max)))))

(defn process-field-retval [{:keys [data errors]}]
  {:data data
   :errors (if (or (nil? errors) (empty? errors)) nil errors)})

(defn make-content-type-or-block-setter [field]
  (let [field-types (:fields field)
        deps (:deps field)
        fields (:fields field)
        fields-keys (map first (:fields field))
        setters (reduce (fn [acc [field-key field-type]]
                          (assoc acc field-key (get-in field [:setters field-type])))
                        {} fields)]
    (fn [block-data]
      (process-field-retval
       (reduce (fn [acc k]
                 (let [acc-data (:data acc)
                       acc-errors (:errors acc)
                       setter (get setters k)
                       {:keys [data errors]} (setter (get block-data k))]
                   {:data (assoc acc-data k data)
                    :errors (if errors (assoc acc-errors k errors) acc-errors)}))
               {:data {} :errors {}} fields-keys)))))

(defn validate-field-type [field]
  (let [v (validator {:field-type [not-empty-validator]})]
    (v field)))
