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

(deftest masks-protect-sculpted-vertices
  (let [mesh (sculpt/sphere-mesh 1 16 8)
        mask-brush (sculpt/brush [1 0 0] 0.7 1 :mask)
        masked (sculpt/apply-stroke mesh mask-brush nil)
        sculpt-brush (sculpt/brush [1 0 0] 0.7 0.5 :inflate)
        original-sculpt (sculpt/apply-stroke mesh sculpt-brush nil)
        protected-sculpt (sculpt/apply-stroke masked sculpt-brush nil)
        most-masked (apply max-key #(nth (:masks masked) %) (range (count (:masks masked))))]
    (is (pos? (nth (:masks masked) most-masked)))
    (is (not= (nth (:positions original-sculpt) most-masked)
              (nth (:positions protected-sculpt) most-masked)))
    (is (every? zero? (:masks (sculpt/clear-mask masked))))
    (is (= 1.0 (first (:masks (sculpt/invert-mask mesh)))))))

(deftest stable-strokes-and-multi-axis-symmetry
  (let [points [[0 0 0] [1 0 0]]
        sampled (sculpt/resample-stroke points 0.25)
        brushes (sculpt/symmetry-brushes (sculpt/brush [1 2 0] 1 0.1 :inflate) [:x :y])]
    (is (= [[0 0 0] [0.25 0.0 0.0] [0.5 0.0 0.0] [0.75 0.0 0.0] [1.0 0.0 0.0]] sampled))
    (is (= #{[1 2 0] [-1 2 0] [1 -2 0] [-1 -2 0]} (set (map :brush/center brushes))))))

(deftest shared-edge-subdivision
  (let [mesh {:positions [[0 0 1] [1 0 0] [0 1 0]] :normals [[0 0 1] [1 0 0] [0 1 0]]
              :masks [0 0.5 1] :indices [0 1 2]}
        subdivided (sculpt/subdivide-mesh mesh)]
    (is (= 6 (count (:positions subdivided))))
    (is (= 12 (count (:indices subdivided))))
    (is (= 6 (count (:masks subdivided))))))
