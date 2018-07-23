(ns content-store.core-test
  (:require [cljs.test :refer-macros [is are deftest testing use-fixtures]]
            [content-store.core :as cs :refer [get-exception-errors]]))

(deftest content-storage-throws-on-field-validation-error
  (let [title {:type :title}]
    (try
      (cs/build-content-storage [title])
      (is false "Shouldn't be here")
      (catch js/Object e
        (is (= {0 {:field-type {:$errors$ {:value nil :failed [:not-empty]}}}}
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
      (is false "Shouldn't be here")
      (catch js/Object e
        (is (= {1 {:dependency {:value :author :failed [:not-circular]}}}
               (get-exception-errors e)))))))


(def content-store-with-inline-fields
  [{:type         :title
    :extends-type :string
    :field-type   :atom
    :constraints  {:required true}}

   {:type         :lead
    :extends-type :string
    :field-type   :atom}

   {:type         :body
    :extends-type :string
    :field-type   :atom}
   
   {:type       :image
    :field-type :block
    :fields     [[:url {:extends-type :string
                        :constraints  {:format :url}
                        :field-type   :atom}]
                 [:caption {:extends-type :string
                            :field-type   :atom}]]}

   {:type         :image-gallery
    :field-type   :collection
    :allowed-type {:field-type :block
                   :fields     [[:url {:extends-type :string
                                       :constraints  {:format :url}
                                       :field-type   :atom}]
                                [:caption {:extends-type :string
                                           :field-type   :atom}]]}}

   {:type          :post-body
    :field-type    :compound
    :allowed-types [{:field-type   :collection
                     :allowed-type {:field-type :block
                                    :fields     [[:url {:extends-type :string
                                                        :constraints  {:format :url}
                                                        :field-type   :atom}]
                                                 [:caption {:extends-type :string
                                                            :field-type   :atom}]]}}
                    {:field-type :atom
                     :extends-type :string}
                    :body]}

   {:type       :post
    :field-type :content-type
    :fields     [[:title :title]
                 [:lead :lead]
                 [:main-image {:field-type :block
                               :fields     [[:url {:extends-type :string
                                                   :constraints  {:format :url}
                                                   :field-type   :atom}]
                                            [:caption {:extends-type :string
                                                       :field-type   :atom}]]}]
                 [:body :post-body]]}])

(deftest extract-inline-fields
  (let [with-extracted (cs/extract-inline-fields content-store-with-inline-fields)]
    (is (= [{:type :post,
             :field-type :content-type,
             :fields
             [[:title :title]
              [:lead :lead]
              [:main-image :field--1183525923]
              [:body :post-body]]}
            {:type :post-body,
             :field-type :compound,
             :allowed-types [:field-2119949618 :field-1073793068 :body]}
            {:type :title,
             :extends-type :string,
             :field-type :atom,
             :constraints {:required true}}
            {:type :lead, :extends-type :string, :field-type :atom}
            {:type :body, :extends-type :string, :field-type :atom}
            {:type :image,
             :field-type :block,
             :fields [[:url :field-336916225] [:caption :field-2036210355]]}
            {:type :image-gallery,
             :field-type :collection,
             :allowed-type :field--1982275402}
            {:field-type :block,
             :fields [[:url :field-95554150] [:caption :field--2077693943]],
             :type :field--1982275402}
            {:extends-type :string,
             :constraints {:format :url},
             :field-type :atom,
             :type :field-95554150}
            {:extends-type :string, :field-type :atom, :type :field--2077693943}
            {:extends-type :string,
             :constraints {:format :url},
             :field-type :atom,
             :type :field-336916225}
            {:extends-type :string, :field-type :atom, :type :field-2036210355}
            {:field-type :collection,
             :allowed-type :field--1076334462,
             :type :field-2119949618}
            {:field-type :block,
             :fields [[:url :field-1596886400] [:caption :field-1994251532]],
             :type :field--1076334462}
            {:extends-type :string,
             :constraints {:format :url},
             :field-type :atom,
             :type :field-1596886400}
            {:extends-type :string, :field-type :atom, :type :field-1994251532}
            {:field-type :atom, :extends-type :string, :type :field-1073793068}
            {:field-type :block,
             :fields [[:url :field--591261993] [:caption :field-607472517]],
             :type :field--1183525923}
            {:extends-type :string,
             :constraints {:format :url},
             :field-type :atom,
             :type :field--591261993}
            {:extends-type :string, :field-type :atom, :type :field-607472517}])
        with-extracted)
    (cs/build-content-storage with-extracted)))
