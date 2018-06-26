(ns content-store.content-store.atom-test
  (:require [cljs.test :refer-macros [is are deftest testing use-fixtures]]
            [content-store.core :as cs]
            [content-store.content-store.atom :as atom]
            [cljsjs.moment]))

(defn make-setter
  ([extends-type] (make-setter extends-type {}))
  ([extends-type additional]
   (let [field (merge additional
                      {:type :testing-field
                       :label "Field"
                       :extends-type extends-type
                       :field-type :atom})
         content-storage (cs/build-content-storage [field])
         expanded-field (cs/expand-field content-storage :testing-field)]
     (cs/make-setter expanded-field))))

(deftest field-setup-validations
  (is (= {:type         {:$errors$ {:value nil :failed [:not-empty]}}
          :extends-type {:$errors$ {:value nil :failed [:valid-atom]}}}
         (atom/validate [{}] {}))))

(deftest field-constraints-applicable-types
  (let [f {:type :title
           :extends-type :string
           :constraints {:range {:min 1 :max 3}}}
        e (atom/validate [f] f)
        failed (get-in e [:constraints :range :$errors$ :failed])]
    (is (= [:applicable-constraint] failed)))
  (let [f {:type :title
           :label "F"
           :extends-type :integer
           :constraints {:format :email}}
        e (atom/validate [f] f)
        failed (get-in e [:constraints :format :$errors$ :failed])]
    (is (= [:applicable-constraint] failed))))

(deftest field-constraints-arguments-required
  (let [f {:type :title
           :extends-type :string
           :constraints {:required "blah"}}
        e (atom/validate [f] f)
        failed (get-in e [:constraints :required :$errors$ :failed])]
    (is (= [:constraint-arguments] failed)))
  (let [f {:type :title
           :extends-type :string
           :constraints {:required true}}
        e (atom/validate [f] f)]
    (is (= {} e)))
  (let [f {:type :title
           :extends-type :string
           :constraints {:required false}}
        e (atom/validate [f] f)]
    (is (= {} e)))
  (let [f {:type :title
           :extends-type :string
           :constraints {:required false}}
        e (atom/validate [f] f)]
    (is (= {} e)))
  (let [f {:type :title
           :extends-type :string
           :constraints {:required false}}
        e (atom/validate [f] f)]
    (is (= {} e)))
  (let [f {:type :title
           :extends-type :string
           :constraints {:required false}}
        e (atom/validate [f] f)]
    (is (= {} e)))
  (let [f {:type :title
           :extends-type :string
           :constraints {:required false}}
        e (atom/validate [f] f)]
    (is (= {} e)))
  (let [f {:type :title
           :extends-type :string
           :constraints {:required false}}
        e (atom/validate [f] f)]
    (is (= {} e)))
  (let [f {:type :title
           :extends-type :string
           :constraints {:required false}}
        e (atom/validate [f] f)]
    (is (= {} e))))

(deftest field-constraints-arguments-predefined-value
  (let [f {:type :title
           :extends-type :string
           :constraints {:predefined-value "test"}}
        e (atom/validate [f] f)
        failed (get-in e [:constraints :predefined-value :$errors$ :failed])]
    (is (= [:constraint-arguments] failed)))
  (let [f {:type :title
           :extends-type :string
           :constraints {:predefined-value []}}
        e (atom/validate [f] f)
        failed (get-in e [:constraints :predefined-value :$errors$ :failed])]
    (is (= [:constraint-arguments] failed)))
  (let [f {:type :title
           :extends-type :string
           :constraints {:predefined-value ["foo" "bar"]}}
        e (atom/validate [f] f)]
    (is (= {} e))))

(deftest field-constraint-arguments-predefined-value-list
  (let [f {:type :some-type
           :extends-type :integer
           :constraints {:predefined-value [1 "A" 2]}}
        e (atom/validate [f] f)]
    (is (= {:constraints
            {:predefined-value
             {1
              {:$errors$
               {:value "A"
                :failed [:predefined-value-type]}}}}}
           e)))
  (let [f {:type :some-type
           :label "F"
           :extends-type :integer
           :constraints {:predefined-value [1 2]}}
        e (atom/validate [f] f)]
    (is (= {} e))))

(deftest field-constraints-arguments-format
  (let [f {:type :title
           :extends-type :string
           :constraints {:format :non-existing-format}}
        e (atom/validate [f] f)
        failed (get-in e [:constraints :format :$errors$ :failed])]
    (is (= [:constraint-arguments] failed)))
  (let [f {:type :title
           :extends-type :string
           :constraints {:format {}}}
        e (atom/validate [f] f)
        failed (get-in e [:constraints :format :$errors$ :failed])]
    (is (= [:constraint-arguments] failed)))
  (let [f {:type :title
           :extends-type :string
           :constraints {:format 1}}
        e (atom/validate [f] f)
        failed (get-in e [:constraints :format :$errors$ :failed])]
    (is (= [:constraint-arguments] failed)))
  (let [f {:type :title
           :extends-type :string
           :constraints {:format :email}}
        e (atom/validate [f] f)]
    (is (= {} e)))
  (let [f {:type :title
           :extends-type :string
           :constraints {:format "email"}}
        e (atom/validate [f] f)]
    (is (= {} e))))

(deftest field-constraints-arguments-size
  (let [f {:type :title
           :extends-type :string
           :constraints {:size 1}}
        e (atom/validate [f] f)
        failed (get-in e [:constraints :size :$errors$ :failed])]
    (is (= [:constraint-arguments] failed)))
  (let [f {:type :title
           :extends-type :string
           :constraints {:size {}}}
        e (atom/validate [f] f)
        failed (get-in e [:constraints :size :$errors$ :failed])]
    (is (= [:constraint-arguments] failed)))
  (let [f {:type :title
           :extends-type :string
           :constraints {:size {:min "test" :max "test"}}}
        e (atom/validate [f] f)
        failed (get-in e [:constraints :size :$errors$ :failed])]
    (is (= [:constraint-arguments] failed)))
  (let [f {:type :title
           :extends-type :string
           :constraints {:size {:min 1 :max 1}}}
        e (atom/validate [f] f)
        failed (get-in e [:constraints :size :$errors$ :failed])]
    (is (= [:constraint-arguments] failed)))
  (let [f {:type :title
           :extends-type :string
           :constraints {:size {:min 1}}}
        e (atom/validate [f] f)]
    (is (= {} e)))
  (let [f {:type :title
           :extends-type :string
           :constraints {:size {:min "1"}}}
        e (atom/validate [f] f)]
    (is (= {} e)))
  (let [f {:type :title
           :extends-type :string
           :constraints {:size {:min 1 :max 2}}}
        e (atom/validate [f] f)]
    (is (= {} e)))
  (let [f {:type :title
           :extends-type :string
           :constraints {:size {:max 1}}}
        e (atom/validate [f] f)]
    (is (= {} e))))

(deftest field-constraints-arguments-range
  (let [f {:type :title
           :extends-type :float
           :constraints {:range 1}}
        e (atom/validate [f] f)
        failed (get-in e [:constraints :range :$errors$ :failed])]
    (is (= [:constraint-arguments] failed)))
  (let [f {:type :title
           :extends-type :float
           :constraints {:range {}}}
        e (atom/validate [f] f)
        failed (get-in e [:constraints :range :$errors$ :failed])]
    (is (= [:constraint-arguments] failed)))
  (let [f {:type :title
           :extends-type :float
           :constraints {:range {:min "test" :max "test"}}}
        e (atom/validate [f] f)
        failed (get-in e [:constraints :range :$errors$ :failed])]
    (is (= [:constraint-arguments] failed)))
  (let [f {:type :title
           :extends-type :float
           :constraints {:range {:min 1 :max 1}}}
        e (atom/validate [f] f)
        failed (get-in e [:constraints :range :$errors$ :failed])]
    (is (= [:constraint-arguments] failed)))
  (let [f {:type :title
           :extends-type :float
           :constraints {:range {:min 1}}}
        e (atom/validate [f] f)]
    (is (= {} e)))
  (let [f {:type :title
           :extends-type :float
           :constraints {:range {:min "1"}}}
        e (atom/validate [f] f)]
    (is (= {} e)))
  (let [f {:type :title
           :extends-type :float
           :constraints {:range {:min 1 :max 2}}}
        e (atom/validate [f] f)]
    (is (= {} e)))
  (let [f {:type :title
           :extends-type :float
           :constraints {:range {:max 1}}}
        e (atom/validate [f] f)]
    (is (= {} e)))
  (let [f {:type :title
           :extends-type :float
           :constraints {:range {:min 1.19 :max 1.2}}}
        e (atom/validate [f] f)]
    (is (= {} e))))


(deftest string-field-test
  (let [setter (make-setter :string)]
    (is (= {:data "Title"
            :errors nil}
           (setter "Title")))
    (is (= {:data ""
            :errors nil}
           (setter nil)))
    (is (= {:data 1
            :errors {:$errors$ {:value 1 :failed [:string-type]}}}
           (setter 1)))
    (is (= {:data true
            :errors {:$errors$ {:value true :failed [:string-type]}}}
           (setter true)))))

(deftest text-field-test
  (let [setter (make-setter :string)]
    (is (= {:data "Title"
            :errors nil}
           (setter "Title")))
    (is (= {:data 1
            :errors {:$errors$ {:value 1 :failed [:string-type]}}}
           (setter 1)))
    (is (= {:data true
            :errors {:$errors$ {:value true :failed [:string-type]}}}
           (setter true)))))

(deftest date-field-test
  (let [setter (make-setter :date)]
    (let [{:keys [data errors]} (setter "2012-12-12")]
      (is (nil? errors))
      (is (= "2012-12-12" (.format data "YYYY-MM-DD"))))
    (let [{:keys [data errors]} (setter (js/Date.))]
      (is (nil? errors))
      (is (= (.format (js/moment) "YYYY-MM-DD")
             (.format data "YYYY-MM-DD"))))
    (is (= {:data "2017-02-29"
            :errors {:$errors$ {:value "2017-02-29" :failed [:date-type]}}}
           (setter "2017-02-29")))
    (is (= {:data "test"
            :errors {:$errors$ {:value "test" :failed [:date-type]}}}
           (setter "test")))

    (is (= {:data true
            :errors {:$errors$ {:value true :failed [:date-type]}}}
           (setter true)))))

(deftest datetime-field-test
  (let [setter (make-setter :datetime)
        {:keys [data errors]} (setter "2012-12-12T22:22")]
    (is (nil? errors))
    (is (= (.format data "YYYY-MM-DDTHH:mm:ss.SSS[Z]")
           "2012-12-12T22:22:00.000Z"))))

(deftest integer-field-test
  (let [setter (make-setter :integer)]
    (is (= {:data 1
            :errors nil}
           (setter 1)))
    (is (= {:data 1
            :errors nil}
           (setter "1")))
    (is (= {:data 1
            :errors nil}
           (setter "1a")))
    (is (= {:data 1
            :errors nil}
           (setter "1.0")))
    (is (= {:data 1.25
            :errors {:$errors$ {:value 1.25 :failed [:integer-type]}}}
           (setter 1.25)))
    (is (= {:data "1.25"
            :errors {:$errors$ {:value "1.25" :failed [:integer-type]}}}
           (setter "1.25")))
    (is (= {:data "a1"
            :errors {:$errors$ {:value "a1" :failed [:integer-type]}}}
           (setter "a1")))
    (is (= {:data {:foo :bar}
            :errors {:$errors$ {:value {:foo :bar} :failed [:integer-type]}}}
           (setter {:foo :bar})))))

(deftest float-field-test
  (let [setter (make-setter :float)]
    (is (= {:data 1
            :errors nil}
           (setter 1.0)))
    (is (= {:data 1
            :errors nil}
           (setter "1")))
    (is (= {:data 1
            :errors nil}
           (setter "1a")))
    (is (= {:data 1
            :errors nil}
           (setter "1.0")))
    (is (= {:data 1.25
            :errors nil}
           (setter 1.25)))
    (is (= {:data 1.25
            :errors nil}
           (setter "1.25")))
    (is (= {:data "a1"
            :errors {:$errors$ {:value "a1" :failed [:float-type]}}}
           (setter "a1")))
    (is (= {:data {:foo :bar}
            :errors {:$errors$ {:value {:foo :bar} :failed [:float-type]}}}
           (setter {:foo :bar})))))

(deftest boolean-field-test
  (let [setter (make-setter :boolean)]
    (is (= {:data true
            :errors nil}
           (setter true)))
    (is (= {:data true
            :errors nil}
           (setter 1)))
    (is (= {:data true
            :errors nil}
           (setter "1")))
    (is (= {:data true
            :errors nil}
           (setter "t")))
    (is (= {:data true
            :errors nil}
           (setter "true")))
    (is (= {:data true
            :errors nil}
           (setter "TRUE")))
    (is (= {:data true
            :errors nil}
           (setter "T")))
    (is (= {:data true
            :errors nil}
           (setter "True")))
    (is (= {:data false
            :errors nil}
           (setter false)))
    (is (= {:data false
            :errors nil}
           (setter 0)))
    (is (= {:data false
            :errors nil}
           (setter "0")))
    (is (= {:data false
            :errors nil}
           (setter "f")))
    (is (= {:data false
            :errors nil}
           (setter "false")))
    (is (= {:data false
            :errors nil}
           (setter "FALSE")))
    (is (= {:data false
            :errors nil}
           (setter "F")))
    (is (= {:data false
            :errors nil}
           (setter "False")))
    (is (= {:data "random"
            :errors {:$errors$ {:value "random" :failed [:boolean-type]}}}
           (setter "random")))))

(deftest location-field-test
  (let [setter (make-setter :location)]
    (is (= {:data {:longitude 1.23 :latitude 4.56}
            :errors nil}
           (setter {:longitude 1.23 :latitude 4.56})))
    (is (= {:data {:longitude 1.23 :latitude 4.56}
            :errors nil}
           (setter {:longitude "1.23" :latitude "4.56"})))
    (is (= {:data {:longitude 1.23 :latitude 90.00000001}
            :errors {:$errors$ {:value {:longitude 1.23 :latitude 90.00000001} :failed [:location-type]}}}
           (setter {:longitude 1.23 :latitude 90.00000001})))
    (is (= {:data {:longitude 1.23 :latitude -90.00000001}
            :errors {:$errors$ {:value {:longitude 1.23 :latitude -90.00000001} :failed [:location-type]}}}
           (setter {:longitude 1.23 :latitude -90.00000001})))
    (is (= {:data {:longitude 180.00000001 :latitude 4.56}
            :errors {:$errors$ {:value {:longitude 180.00000001 :latitude 4.56} :failed [:location-type]}}}
           (setter {:longitude 180.00000001 :latitude 4.56})))
    (is (= {:data {:longitude -180.00000001 :latitude 4.56}
            :errors {:$errors$ {:value {:longitude -180.00000001 :latitude 4.56} :failed [:location-type]}}}
           (setter {:longitude -180.00000001 :latitude 4.56})))))

(deftest required-constraint-validation
  (let [setter (make-setter :string {:constraints {:required true}})]
    (is (= {:data ""
            :errors {:$errors$ {:value "" :failed [:required-constraint]}}}
           (setter "")))
    (is (= {:data ""
            :errors {:$errors$ {:value nil :failed [:required-constraint]}}}
           (setter nil)))
    (is (= {:data "Foo"
            :errors nil}
           (setter "Foo")))))

(deftest predefined-value-validation
  (let [setter (make-setter :string {:constraints {:predefined-value ["foo" "bar"]}})]
    (is (= {:data "baz"
            :errors {:$errors$ {:value "baz" :failed [:predefined-value-constraint]}}}
           (setter "baz")))
    (is (= {:data "foo"
            :errors nil}
           (setter "foo")))))

(deftest format-email-validation
  (let [setter (make-setter :string {:constraints {:format :email}})]
    (is (= {:data "example"
            :errors {:$errors$ {:value "example" :failed [:format-constraint]}}}
           (setter "example")))
    (is (= {:data "example@example.com"
            :errors nil}
           (setter "example@example.com")))
    (is (= {:data "example+example@example.com"
            :errors nil}
           (setter "example+example@example.com")))
    (is (= {:data "other.email-with-dash@example.com"
            :errors nil}
           (setter "other.email-with-dash@example.com")))
    (is (= {:data "disposable.style.email.with+symbol@example.com"
            :errors nil}
           (setter "disposable.style.email.with+symbol@example.com")))
    (is (= {:data "example@s.solutions"
            :errors nil}
           (setter "example@s.solutions")))
    (is (= {:data "example@s.subdomain.solutions"
            :errors nil}
           (setter "example@s.subdomain.solutions")))))

(deftest format-url-validation
  (let [setter (make-setter :string {:constraints {:format :url}})]
    (is (= {:data "example"
            :errors {:$errors$ {:value "example" :failed [:format-constraint]}}}
           (setter "example")))
    (is (= {:data "http://example.com"
            :errors nil}
           (setter "http://example.com")))
    (is (= {:data "https://example.com"
            :errors nil}
           (setter "https://example.com")))
    (is (= {:data "https://example.com/"
            :errors nil}
           (setter "https://example.com/")))
    (is (= {:data "ftp://example.com"
            :errors nil}
           (setter "ftp://example.com")))
    (is (= {:data "https://example.com/test/?foo#bar"
            :errors nil}
           (setter "https://example.com/test/?foo#bar")))
    (is (= {:data "https://example.com/index.html"
            :errors nil}
           (setter "https://example.com/index.html")))
    (is (= {:data "https://example.com/index.html?foo=a&bar[baz]=qux"
            :errors nil}
           (setter "https://example.com/index.html?foo=a&bar[baz]=qux")))
    (is (= {:data "http://localhost/index.html?foo=a&bar[baz]=qux"
            :errors nil}
           (setter "http://localhost/index.html?foo=a&bar[baz]=qux")))
    (is (= {:data "http://localhost:8888/index.html?foo=a&bar[baz]=qux"
            :errors nil}
           (setter "http://localhost:8888/index.html?foo=a&bar[baz]=qux")))))

(deftest format-us-phone-number-validation
    (let [setter (make-setter :string {:constraints {:format :us-phone-number}})]
      (is (= {:data "123"
              :errors {:$errors$ {:value "123" :failed [:format-constraint]}}}
             (setter "123")))
      (is (= {:data "(541) 754-3010"
              :errors nil}
             (setter "(541) 754-3010")))
      (is (= {:data "541-754-3010"
              :errors nil}
             (setter "541-754-3010")))))

(deftest format-us-zip-code-validation
    (let [setter (make-setter :string {:constraints {:format :us-zip-code}})]
      (is (= {:data "123"
              :errors {:$errors$ {:value "123" :failed [:format-constraint]}}}
             (setter "123")))
      (is (= {:data "02201"
              :errors nil}
             (setter "02201")))
      (is (= {:data "02201-1020"
              :errors nil}
             (setter "02201-1020")))))

(deftest format-alpha-validation
    (let [setter (make-setter :string {:constraints {:format :alpha}})]
      (is (= {:data "123"
              :errors {:$errors$ {:value "123" :failed [:format-constraint]}}}
             (setter "123")))
      (is (= {:data "test "
              :errors {:$errors$ {:value "test " :failed [:format-constraint]}}}
             (setter "test ")))
      (is (= {:data "test 1 "
              :errors {:$errors$ {:value "test 1 " :failed [:format-constraint]}}}
             (setter "test 1 ")))
      (is (= {:data "test"
              :errors nil}
             (setter "test")))
      (is (= {:data "TEST"
              :errors nil}
             (setter "TEST")))))

(deftest format-alnum-validation
    (let [setter (make-setter :string {:constraints {:format :alnum}})]
      (is (= {:data "123!"
              :errors {:$errors$ {:value "123!" :failed [:format-constraint]}}}
             (setter "123!")))
      (is (= {:data "test "
              :errors {:$errors$ {:value "test " :failed [:format-constraint]}}}
             (setter "test ")))
      (is (= {:data "test 1 "
              :errors {:$errors$ {:value "test 1 " :failed [:format-constraint]}}}
             (setter "test 1 ")))
      (is (= {:data "test"
              :errors nil}
             (setter "test")))
      (is (= {:data "TEST"
              :errors nil}
             (setter "TEST")))
      (is (= {:data "123"
              :errors nil}
             (setter "123")))
      (is (= {:data "TEST123"
              :errors nil}
             (setter "TEST123")))))
