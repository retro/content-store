(ns content-store.core-test
  (:require [cljs.test :refer-macros [is are deftest testing use-fixtures]]
            [content-store.core :as cs :refer [get-exception-errors]]))

(deftest content-storage-throws-on-field-validation-error
  (let [title {:type :title}]
    (try
      (cs/build-content-storage [title])
      (catch js/Object e
        (is (= {0 {:field-type {:$errors$ {:value nil :failed [:not-empty]}}}}
               (get-exception-errors e))))))
  (let [title {:type :title
               :extends-type :string
               :field-type :atom}]
    (try
      (cs/build-content-storage [title])
      (catch js/Object e
        (is (= {0 {:label {:$errors$ {:value nil :failed [:not-empty]}}}}
               (get-exception-errors e)))))))

(deftest content-storage-throws-on-circular-deps
  (let [author {:type :author
                :field-type :block
                :label "Author"
                :fields [[:image :image]]}
        image {:type :image
               :field-type :block
               :label "Image"
               :fields [[:author :author]]}]
    (try
      (cs/build-content-storage [author image])
      (catch js/Object e
        (is (= {1 {:dependency {:value :author :failed [:not-circular]}}}
               (get-exception-errors e)))))))

