(ns content-store.content-store.content-type-test
  (:require [cljs.test :refer-macros [is are deftest testing use-fixtures]]
            [content-store.core :as cs]
            [content-store.content-store.content-type :as content-type]
            [cljs.pprint :refer [pprint]]))

(defn make-content-type []
  (let [fields          [{:type         :title
                          :extends-type :string
                          :field-type   :atom
                          :constraints  {:required true}}
                         {:type         :lead
                          :extends-type :string
                          :field-type   :atom}
                         {:type         :body
                          :extends-type :string
                          :field-type   :atom}
                         {:type         :image-url
                          :extends-type :string
                          :constraints  {:format :url}
                          :field-type   :atom}
                         {:type         :image-caption
                          :extends-type :string
                          :field-type   :atom}
                         {:type       :image
                          :field-type :block
                          :fields     [[:url :image-url]
                                       [:caption :image-caption]]}
                         {:type         :image-gallery
                          :field-type   :collection
                          :allowed-type :image}
                         {:type          :post-body
                          :field-type    :compound
                          :allowed-types [:image-gallery :body]}
                         {:type       :post
                          :field-type :content-type
                          :fields     [[:title :title]
                                       [:lead :lead]
                                       [:main-image :image]
                                       [:body :post-body]]}]
        content-storage (cs/build-content-storage fields)
        expanded-field  (cs/expand-field content-storage :post)]
    (cs/make-setter expanded-field)))

(def post-data
  {:title      "Post Title"
   :lead       "Here's some text"
   :main-image {:url     "http://example.com"
                :caption "Main Image Caption"}
   :body       [{:_type :body :_payload "First paragraph"}
                {:_type :image-gallery
                 :_payload [{:url     "http://example.com"
                             :caption "First image"}]}
                {:_type :body :_payload "Second paragraph"}]})

(deftest content-type
  (let [post (make-content-type)]
    (is (= {:data post-data
            :errors nil}
           (post post-data)))))

(defn make-stringly-content-type []
  (let [fields          [{:type         "title"
                          :extends-type "string"
                          :field-type   "atom"
                          :constraints  {:required true}}
                         {:type         "lead"
                          :extends-type "string"
                          :field-type   "atom"}
                         {:type         "body"
                          :extends-type "string"
                          :field-type   "atom"}
                         {:type         "image-url"
                          :extends-type "string"
                          :constraints  {:format "url"}
                          :field-type   "atom"}
                         {:type         "image-caption"
                          :extends-type "string"
                          :field-type   "atom"}
                         {:type       "image"
                          :field-type "block"
                          :fields     [["url" "image-url"]
                                       ["caption" "image-caption"]]}
                         {:type         "image-gallery"
                          :field-type   "collection"
                          :allowed-type "image"}
                         {:type          "post-body"
                          :field-type    "compound"
                          :allowed-types ["image-gallery" "body"]}
                         {:type       "post"
                          :field-type "content-type"
                          :fields     [["title" "title"]
                                       ["lead" "lead"]
                                       ["main-image" "image"]
                                       ["body" "post-body"]]}]
        content-storage (cs/build-content-storage fields)
        expanded-field  (cs/expand-field content-storage :post)]
    (cs/make-setter expanded-field)))

(def stringly-post-data
  {:title      "Post Title"
   :lead       "Here's some text"
   :main-image {:url     "http://example.com"
                :caption "Main Image Caption"}
   :body       [{:_type "body" :_payload "First paragraph"}
                {:_type "image-gallery"
                 :_payload [{:url     "http://example.com"
                             :caption "First image"}]}
                {:_type "body" :_payload "Second paragraph"}]})


(deftest stringly-content-type
  (let [post (make-stringly-content-type)]
    (is (= {:data post-data
            :errors nil}
           (post stringly-post-data)))))
