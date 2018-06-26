(ns content-store.content-store.collection-test
  (:require [cljs.test :refer-macros [is are deftest testing use-fixtures]]
            [content-store.core :as cs]
            [content-store.content-store.collection :as collection]))


(defn make-simple-collection []
  (let [title           {:type         :title
                         :extends-type :string
                         :field-type   :atom
                         :constraints  {:required true}}
        titles          {:type         :titles
                         :field-type   :collection
                         :allowed-type :title}
        content-storage (cs/build-content-storage [title titles])
        expanded-field  (cs/expand-field content-storage :titles)]
    (cs/make-setter expanded-field)))

(deftest simple-collection
  (let [collection (make-simple-collection)]
    (is (= {:errors
            {0 {:$errors$ {:value nil, :failed [:required-constraint]}}
             3 {:$errors$ {:value nil, :failed [:required-constraint]}}}
            :data ["" "Some Test" "Some Test 2" ""]}
           (collection [nil "Some Test" "Some Test 2" nil])))
    (is (= {:errors nil
            :data ["Some Test" "Some Test 2"]}
           (collection ["Some Test" "Some Test 2"])))))

(defn make-complex-collection []
  (let [image-caption   {:type         :image-caption
                         :extends-type :string
                         :field-type   :atom
                         :constraints  {:required true}}
        image-url       {:type         :image-url
                         :extends-type :string
                         :field-type   :atom
                         :constraints  {:format :url}}
        image           {:type       :image
                         :fields     [[:caption :image-caption]
                                      [:url :image-url]]
                         :field-type :block}
        image-gallery   {:type         :image-gallery
                         :field-type   :collection
                         :allowed-type :image}
        title           {:type         :title
                         :extends-type :string
                         :field-type   :atom
                         :constraints  {:required true}}
        post            {:type       :post
                         :field-type :block
                         :fields     [[:gallery :image-gallery]
                                      [:title :title]]}
        posts           {:type         :posts
                         :field-type   :collection
                         :allowed-type :post}
        content-storage (cs/build-content-storage
                         [image-caption image-url image image-gallery title post posts])
        expanded-field  (cs/expand-field content-storage :posts)]
    (cs/make-setter expanded-field)))

(deftest complex-collection
  (let [collection (make-complex-collection)]
    (is (= {:errors
            {0
             {:gallery
              {0
               {:caption {:$errors$ {:value nil, :failed [:required-constraint]}}
                :url {:$errors$ {:value nil, :failed [:format-constraint]}}}
               2
               {:caption {:$errors$ {:value nil, :failed [:required-constraint]}}
                :url {:$errors$ {:value nil, :failed [:format-constraint]}}}}}}
            :data
            [{:title "Some Title"
              :gallery
              [{:caption "", :url ""}
               {:caption "Image", :url "http://example.com/image.jpg"}
               {:caption "", :url ""}]}]}
           (collection [{:title "Some Title"
                         :gallery [{:caption nil :url nil}
                                   {:caption "Image" :url "http://example.com/image.jpg"}
                                   nil]}])))
    (is (= {:data [{:title "Some Title"
                    :gallery [{:caption "Caption" :url "http://example.com/image.jpg"}]}]
            :errors nil}
           (collection [{:title "Some Title"
                         :gallery [{:caption "Caption" :url "http://example.com/image.jpg"}]}])))))
