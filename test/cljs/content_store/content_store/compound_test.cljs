(ns content-store.content-store.compound-test
  (:require [cljs.test :refer-macros [is are deftest testing use-fixtures]]
            [content-store.core :as cs]
            [content-store.content-store.compound :as compund]))

(defn make-simple-compound []
  (let [title     {:type         :title
                   :extends-type :string
                   :field-type   :atom
                   :constraints  {:required true}}
        body-text {:type         :body
                   :extends-type :string
                   :field-type   :atom
                   :constraints  {:required true}}
        url       {:type         :url
                   :extends-type :string
                   :field-type   :atom
                   :constraints  {:format :url}}
        post      {:type          :post
                   :field-type    :compound
                   :allowed-types [:title :body :url]}
        content-storage (cs/build-content-storage [title body-text url post])
        expanded-field (cs/expand-field content-storage :post)]
    (cs/make-setter expanded-field)))

(deftest simple-compound
  (let [compound (make-simple-compound)]
    (is (= {:errors
            {0 {:_payload {:$errors$ {:value "", :failed [:required-constraint]}}}
             2 {:_payload {:$errors$ {:value "", :failed [:format-constraint]}}}}
            :data
            [{:_type :title, :_payload ""}
             {:_type :body, :_payload "Body #1"}
             {:_type :url, :_payload ""}]}
           (compound [{:_type :title
                       :_payload ""}
                      {:_type :body
                       :_payload "Body #1"}
                      {:_type :url
                       :_payload ""}])))
    (is (= {:errors nil
            :data [{:_type :title :_payload "Title"}
                   {:_type :body :_payload "Body 1"}
                   {:_type :url :_payload "http://example.com"}]}
           (compound [{:_type :title :_payload "Title"}
                      {:_type :body :_payload "Body 1"}
                      {:_type :url :_payload "http://example.com"}])))))


(defn make-complex-compound []
  (let [image-url       {:type         :image-url
                         :extends-type :string
                         :field-type   :atom
                         :constraints  {:format :url}}
        image-caption   {:type         :image-caption
                         :extends-type :string
                         :field-type   :atom
                         :constraints  {:required true}}
        image           {:type       :image
                         :fields     [[:url :image-url]
                                      [:caption :image-caption]]
                         :field-type :block}
        image-gallery   {:type         :image-gallery
                         :field-type   :collection
                         :allowed-type :image}
        body            {:type         :body
                         :extends-type :string
                         :field-type   :atom
                         :constraints  {:required true}}
        post            {:type          :post
                         :field-type    :compound
                         :allowed-types [:image-gallery :body]}
        content-storage (cs/build-content-storage
                         [image-url image-caption image image-gallery body post])
        expanded-field  (cs/expand-field content-storage :post)]
    (cs/make-setter expanded-field)))

(deftest complex-compound
  (let [compound (make-complex-compound)]
    (is (= {:errors
            {0 {:_payload {:$errors$ {:value nil, :failed [:required-constraint]}}}
             1
             {:_payload
              {0
               {:caption {:$errors$ {:value nil, :failed [:required-constraint]}}
                :url {:$errors$ {:value nil, :failed [:format-constraint]}}}}}}
            :data
            [{:_type :body, :_payload ""}
             {:_type :image-gallery, :_payload [{:caption "", :url ""}]}]}
           (compound [{:_type :body :_payload nil}
                      {:_type :image-gallery :_payload [nil]}])))))
