(ns content-store.core
  (:require [com.stuartsierra.dependency :as dep]
            [forms.validator :refer [validator]]
            [content-store.content-store.atom :as atom]
            [content-store.content-store.block :as block]
            [content-store.content-store.collection :as collection]
            [content-store.content-store.compound :as compound]
            [content-store.content-store.content-type :as content-type]
            [content-store.content-store :as core]
            [clojure.string :as str]))

(def fields-api
  {:atom         {:validate     atom/validate
                  :extract-deps atom/extract-deps
                  :make-setter  atom/make-setter}
   :block        {:validate     block/validate
                  :extract-deps block/extract-deps
                  :make-setter  block/make-setter}
   :collection   {:validate     collection/validate
                  :extract-deps collection/extract-deps
                  :make-setter  collection/make-setter}
   :content-type {:validate     content-type/validate
                  :extract-deps content-type/extract-deps
                  :make-setter  content-type/make-setter}
   :compound     {:validate     compound/validate
                  :extract-deps compound/extract-deps
                  :make-setter  compound/make-setter}})

(defn path->type [path]
  (let [p (flatten path)]
    (keyword (str "field-" (hash p)))))

(declare extract-inline-fields)
(declare extract-inline-fields-from-field)
(declare extract-inline-fields-from-block-field-block)
(declare extract-inline-fields-from-collection-field)
(declare extract-inline-fields-from-compound-field)

(defn extract-inline-fields-from-block-field [field state]
  (let [fields (:fields field)
        path (conj (:path state) (:type field))
        {:keys [field-fields field-extracted-fields]}
        (reduce-kv
         (fn [acc idx [k v]]
           (if (map? v)
             (let [field-path [path [idx k]]
                   field-type (path->type field-path)
                   f (assoc v :type field-type)
                   e (extract-inline-fields-from-field f {:extracted-fields []
                                                          :fields []
                                                          :path field-path})]
               (-> acc
                   (update :field-extracted-fields #(flatten [% (:fields e) (:extracted-fields e)]))
                   (update :field-fields #(conj % [k field-type]))))
             (update acc :field-fields #(conj % [k v]))))
         {:field-fields []
          :field-extracted-fields []}
         fields)
        processed-field (assoc field :fields field-fields)]
    (-> state
        (update :fields #(conj % processed-field))
        (update :extracted-fields #(concat % field-extracted-fields)))))

(defn extract-inline-fields-from-collection-field [field state]
  (let [allowed-type (:allowed-type field)]
    (if (map? allowed-type)
      (let [field-path [(:path state) (:type field) :allowed-type]
            field-type (path->type field-path)
            processed-field (assoc field :allowed-type field-type)
            f (assoc allowed-type :type field-type)
            e (extract-inline-fields-from-field f {:extracted-fields []
                                                   :fields []
                                                   :path field-path})]
        (update state :fields #(flatten [% processed-field (:fields e) (:extracted-fields e)])))
      (update state :fields #(conj % field)))))

(defn extract-inline-fields-from-compound-field [field state]
  (let [fields (:allowed-types field)
        path (conj (:path state) (:type field))
        {:keys [allowed-types field-extracted-fields]}
        (reduce-kv
         (fn [acc idx v]
           (if (map? v)
             (let [field-path [path idx]
                   field-type (path->type field-path)
                   f (assoc v :type field-type)
                   e (extract-inline-fields-from-field f {:extracted-fields []
                                                          :fields []
                                                          :path field-path})]
               (-> acc
                   (update :field-extracted-fields #(flatten [% (:fields e) (:extracted-fields e)]))
                   (update :allowed-types #(conj % field-type))))
             (update acc :allowed-types #(conj % v))))
         {:allowed-types []
          :field-extracted-fields []}
         fields)
        processed-field (assoc field :allowed-types allowed-types)]

    (-> state
        (update :fields #(conj % processed-field))
        (update :extracted-fields #(concat % field-extracted-fields)))))


(defn extract-inline-fields-from-field [field state]
   (case (:field-type field)
     :block (extract-inline-fields-from-block-field field state)
     :collection (extract-inline-fields-from-collection-field field state)
     :content-type (extract-inline-fields-from-block-field field state)
     :compound (extract-inline-fields-from-compound-field field state)
     (update state :fields #(conj % field))))

(defn extract-inline-fields
  ([fields]
   (let [{:keys [fields extracted-fields]}
         (extract-inline-fields fields {:extracted-fields []
                                        :path []
                                        :fields []})]
     (vec (concat fields extracted-fields))))
  ([fields state]
   (reduce-kv
    (fn [m k v]
      (extract-inline-fields-from-field v m))
    state fields)))

(defn validate-field [fields field]
  (let [field-type (:field-type field)
        validator-fn (if field-type (get-in fields-api [field-type :validate]) core/validate-field-type)]
    (validator-fn fields field)))

(defn validate-fields [fields]
  (loop [idx 0
         iter-fields fields
         errors {}]
    (if (seq iter-fields) 
      (let [field (first iter-fields)
            field-errors (validate-field fields field)]
        (recur (inc idx)
               (rest iter-fields)
               (if (empty? field-errors) errors (assoc errors idx field-errors))))
      errors)))

(defn extract-field-deps [field]
  (let [extractor-fn (get-in fields-api [(:field-type field) :extract-deps])]
    (assoc field :deps (extractor-fn field))))

(defn build-graph [fields]
  (let [graph (dep/graph)]
    (try
      (reduce (fn [g {:keys [type deps]}]
                (reduce (fn [g d] (dep/depend g type d)) g deps)) graph fields)
      (catch js/Object e
        (let [error-data (.-data e)]
          (if (= (:reason error-data) :com.stuartsierra.dependency/circular-dependency)
            (let [error-type (:node error-data)
                  error-dep (:dependency error-data)]
              (loop [idx 0
                     fields fields]
                (when (seq fields)
                  (let [field (first fields)]
                    (if (= (:type field) error-type)
                      (throw (ex-info "Circular dependency"
                                      {:errors {idx {:dependency
                                                     {:value error-dep
                                                      :failed [:not-circular]}}}}))
                      (recur (inc idx) (rest fields)))))))
            (throw e)))))))

(defn filter-field-types [field-type fields]
  (filter #(= field-type (:field-type %)) fields))

(defn make-setter [field]
  (let [field-type (:field-type field)
        make-setter-fn (get-in fields-api [field-type :make-setter])]
    (make-setter-fn field)))

(defn expand-field [fields-map type]
  (let [field (get fields-map type)
        field-deps (:deps field)]
    (if (seq field-deps)
      (let [children (reduce (fn [acc f] (assoc acc f (expand-field fields-map f)))
                             {} field-deps)
            setters (reduce (fn [acc [type child]]
                              (assoc acc type (make-setter child))) {} children)]
        (assoc field
               :children children
               :setters setters))
      field)))

(def keywordize-attr-fns
  {:type keyword
   :extends-type keyword
   :field-type keyword
   :allowed-type keyword
   :allowed-types (fn [v] (vec (map keyword v)))
   :fields (fn [v] (vec (map (fn [p] (vec (map keyword p))) v)))})

(defn keywordize-field-attrs [field]
  (let [res (reduce-kv (fn [m k v]
                      (if-let [k-attr-fn (get keywordize-attr-fns k)]
                        (assoc m k (k-attr-fn v))
                        (assoc m k v)))
                    {} field)]
    res))

(defn get-exception-errors [e]
  (:errors (.-data e)))

(defn build-content-storage [raw-fields]
  (let [fields (map keywordize-field-attrs raw-fields)
        errors (validate-fields fields)
        has-errors? (not (empty? errors))]
   (if has-errors?
     (throw (ex-info "Fields failed validations" {:errors errors}))
     (let [fields-with-deps (map extract-field-deps fields)
           fields-map (reduce (fn [acc f] (assoc acc (:type f) f)) {} fields-with-deps)]
       (build-graph fields-with-deps)
       fields-map))))
