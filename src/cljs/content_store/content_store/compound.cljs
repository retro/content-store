(ns content-store.content-store.compound
  (:require [content-store.content-store :refer [not-empty-validator
                                            make-is-unique-type-validator
                                            make-valid-field-deps-validator
                                            process-field-retval]]
            [forms.validator :refer [validator]]))

(defn validate [store field]
  (let [v (validator {:type [not-empty-validator (make-is-unique-type-validator store)]
                      :allowed-types.* [(make-valid-field-deps-validator store :compound)]})]
    (v field)))

(defn extract-deps [field]
  (set (:allowed-types field)))

(defn make-setter [field]
  (fn [data]
    (process-field-retval
     (reduce-kv (fn [acc idx {:keys [_type _payload]}]
                  (let [_type (keyword _type)
                        acc-data (:data acc)
                        acc-errors (:errors acc)
                        setter (get-in field [:setters _type])
                        {:keys [data errors]} (setter _payload)]
                    {:data (conj acc-data {:_type _type :_payload data})
                     :errors (if errors (assoc-in acc-errors [idx :_payload] errors) acc-errors)}))
                {:data [] :errors {}} data))))
