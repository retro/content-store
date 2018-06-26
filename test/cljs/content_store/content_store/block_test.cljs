(ns content-store.content-store.block-test
  (:require [cljs.test :refer-macros [is are deftest testing use-fixtures]]
            [content-store.core :as cs]
            [content-store.content-store.block :as block]))

(defn make-simple-block []
  (let [title           {:type         :title
                         :extends-type :string
                         :field-type   :atom
                         :constraints  {:required         true
                                        :predefined-value ["This is a cool title"]}}
        post            {:type       :post
                         :fields     [[:title :title]]
                         :field-type :block}
        content-storage (cs/build-content-storage [title post])
        expanded-field  (cs/expand-field content-storage :post)]
    (cs/make-setter expanded-field)))

(deftest simple-block
  (let [block (make-simple-block)]
    (is (= {:errors
            {:title
             {:$errors$
              {:value "Test", :failed [:predefined-value-constraint]}}}
            :data {:title "Test"}}
           (block {:title "Test"})))
    (is (= {:errors nil
            :data {:title "This is a cool title"}}
           (block {:title "This is a cool title"})))))

(defn make-nested-block []
  (let [title           {:type         :title
                         :extends-type :string
                         :field-type   :atom
                         :constraints  {:required true}}
        image-caption   {:type         :image-caption
                         :extends-type :string
                         :field-type   :atom
                         :constraints  {:required true}}
        image-url       {:type         :image-url
                         :extends-type :string
                         :field-type   :atom
                         :constraints  {:required true
                                        :format   :url}}
        image           {:type       :image
                         :fields     [[:url :image-url]
                                      [:caption :image-caption]]
                         :field-type :block}
        post            {:type       :post
                         :fields     [[:title :title]
                                      [:image :image]]
                         :field-type :block}
        content-storage (cs/build-content-storage [title image-caption image-url image post])
        expanded-field  (cs/expand-field content-storage :post)]
    (cs/make-setter expanded-field)))

(deftest nested-block
  (let [block (make-nested-block)]
    (is (= {:errors
            {:title {:$errors$ {:value nil, :failed [:required-constraint]}}
             :image
             {:url
              {:$errors$ {:value nil, :failed [:required-constraint :format-constraint]}}
              :caption {:$errors$ {:value nil, :failed [:required-constraint]}}}}
            :data {:title "", :image {:url "", :caption ""}}}
           (block {})))
    (is (= {:data {:title "Test"
                   :image {:url "http://example.com/image.jpg"
                           :caption "Example Image"}}
            :errors nil}
           (block {:title "Test"
                   :image {:url "http://example.com/image.jpg"
                           :caption "Example Image"}})))))

(defn make-block-with-same-types []
  (let [image-caption   {:type         :image-caption
                         :extends-type :string
                         :field-type   :atom
                         :constraints  {:required true}}
        image-url       {:type         :image-url
                         :extends-type :string
                         :field-type   :atom
                         :constraints  {:required true
                                        :format   :url}}
        image           {:type       :image
                         :fields     [[:url :image-url]
                                      [:caption :image-caption]]
                         :field-type :block}
        post            {:type       :post
                         :fields     [[:main-image :image]
                                      [:thumb-image :image]]
                         :field-type :block}
        content-storage (cs/build-content-storage [image-caption image-url image post])
        expanded-field  (cs/expand-field content-storage :post)]
    (cs/make-setter expanded-field)))

(deftest block-with-same-types
  (let [setter (make-block-with-same-types)]
    (is (= {:data {:main-image {:url "http://example.com/main"
                                :caption "Main"}
                   :thumb-image {:url "http://example.com/thumb"
                                 :caption "Thumb"}}
            :errors nil}
           (setter {:main-image {:url "http://example.com/main"
                                 :caption "Main"}
                    :thumb-image {:url "http://example.com/thumb"
                                  :caption "Thumb"}})))))
