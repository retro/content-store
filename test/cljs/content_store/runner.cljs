(ns content-store.runner
    (:require [doo.runner :refer-macros [doo-tests]]
              [content-store.core-test]
              [content-store.content-store.atom-test]
              [content-store.content-store.block-test]
              [content-store.content-store.collection-test]
              [content-store.content-store.compound-test]
              [content-store.content-store.content-type-test]))

(doo-tests 'content-store.core-test
           'content-store.content-store.atom-test
           'content-store.content-store.block-test
           'content-store.content-store.collection-test
           'content-store.content-store.compound-test
           'content-store.content-store.content-type-test)
