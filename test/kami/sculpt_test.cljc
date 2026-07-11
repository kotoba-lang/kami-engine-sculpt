(ns kami.sculpt-test (:require [clojure.test :refer [deftest is]] [kami.sculpt :as sculpt]))
(deftest sphere-topology
  (let [m (sculpt/sphere-mesh 1 16 8)]
    (is (= 153 (count (:positions m))))
    (is (= 768 (count (:indices m))))
    (is (= (count (:positions m)) (count (:normals m))))))
(deftest brush-deforms-with-falloff-and-symmetry
  (let [m (sculpt/sphere-mesh 1 16 8) b (sculpt/brush [1 0 0] 0.8 0.25 :inflate)
        one (sculpt/apply-stroke m b nil) both (sculpt/apply-stroke m b :x)]
    (is (not= (:positions m) (:positions one)))
    (is (> (count (filter false? (map = (:positions m) (:positions both))))
           (count (filter false? (map = (:positions m) (:positions one))))))))
