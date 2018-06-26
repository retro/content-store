(ns content-store.content-store.collection
  (:require [content-store.content-store :refer [not-empty-validator
                                            make-is-unique-type-validator
                                            make-valid-field-deps-validator
                                            process-field-retval]]
            [forms.validator :refer [validator]]))

(defn validate [store field]
  (let [v (validator {:type [not-empty-validator (make-is-unique-type-validator store)]
                      :allowed-type [(make-valid-field-deps-validator store :collection)]})]
    (v field)))

(defn extract-deps [field]
  (set [(:allowed-type field)]))

(defn make-setter [field]
  (let [allowed-type (:allowed-type field)
        setter (get-in field [:setters allowed-type])]
    (fn [data]
      (process-field-retval
       (reduce-kv (fn [acc idx v]
                    (let [acc-data (:data acc)
                          acc-errors (:errors acc)
                          {:keys [data errors]} (setter v)]
                      {:data (conj acc-data data)
                       :errors (if errors (assoc acc-errors idx errors) acc-errors)}))
                  {:data [] :errors {}} data)))))
