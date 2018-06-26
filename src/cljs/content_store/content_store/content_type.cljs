(ns content-store.content-store.content-type
  (:require [content-store.content-store :refer [not-empty-validator
                                            make-is-unique-type-validator
                                            make-valid-field-deps-validator
                                            make-content-type-or-block-setter]]
            [forms.validator :refer [validator]]))

(defn validate [store field]
  (let [v (validator {:type [not-empty-validator (make-is-unique-type-validator store)]
                      :fields.*.1 [(make-valid-field-deps-validator store :content-type)]})]
    (v field)))

(defn extract-deps [field]
  (let [fields (:fields field)]
    (set (map (fn [[k v]] v) fields))))

(def make-setter make-content-type-or-block-setter)
