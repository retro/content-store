(ns content-store.content-store.atom.constraints
  (:require [content-store.content-store :as core]
            [content-store.content-store.atom.types :as types]
            [forms.validator :refer [validator]]))

(defn make-predefined-value-validator [values]
  (let [values-set (set values)]
    (fn [v _ _]
      (contains? values-set v))))

(def format-regexps
  {:email
   (js/RegExp. "[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?")

   :url
   (js/RegExp. "((([A-Za-z]{3,9}:(?://)?)(?:[-;:&=\\+\\$,\\w]+@)?[A-Za-z0-9.-]+|(?:www.|[-;:&=\\+\\$,\\w]+@)[A-Za-z0-9.-]+)((?:/[\\+~%/.\\w-_]*)?\\??(?:[-\\+=&;%@.\\w_]*)#?(?:[\\w]*))?)")

   :us-phone-number
   (js/RegExp. "^(\\([0-9]{3}\\)\\s?|[0-9]{3}-)[0-9]{3}-[0-9]{4}$")

   :us-zip-code
   (js/RegExp. "^[0-9]{5}(?:-[0-9]{4})?$")

   :alpha
   (js/RegExp. "^[a-zA-Z]+$")

   :alnum
   (js/RegExp. "^[a-zA-Z0-9]+$")})

(defn make-format-validation [format]
  (let [format-regexp (get format-regexps (keyword format))]
    (fn [v _ _]
      (.test format-regexp v))))

(defn make-range-validation [{:keys [min max]}]
  (let [min (or min (- js/Infinity))
        max (or max js/Infinity)]
    (fn [v _ _]
      (<= min v max))))

(def constraints
  {:required         {:make-validation  (fn [] core/not-empty-validator-fn)
                      :applicable-types types/valid-atom-types}
   :predefined-value {:make-validation  make-predefined-value-validator
                      :applicable-types #{:string :integer :float :location}}
   :format           {:make-validation  make-format-validation
                      :applicable-types #{:string}}
   :size             {:make-validation  core/make-size-validation
                      :applicable-types #{:string}}
   :range            {:make-validation  make-range-validation
                      :applicable-types #{:integer :float}}})

(defn build-constraint-validation [[constraint args]]
  (let [make-validation (get-in constraints [constraint :make-validation])]
    [(keyword (str (name constraint) "-constraint"))
     (make-validation args)]))

(defn build-constraints-validator [field]
  (let [field-constraints (:constraints field)
        validations (map build-constraint-validation field-constraints)]
    (validator {:__ validations})))

(defn validate-field-constraints [field data]
  (let [constraints-validator (build-constraints-validator field)
        errors (constraints-validator {:__ data})]
    (get-in errors [:__ :$errors$])))
