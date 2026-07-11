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

(deftest non-destructive-sculpt-layers
  (let [base (sculpt/sphere-mesh 1 16 8) doc (sculpt/sculpt-document base)
        first-stroke (sculpt/apply-layer-stroke doc (sculpt/brush [1 0 0] 0.8 0.2 :inflate) nil)
        with-layer (sculpt/add-layer first-stroke "Wrinkles")
        layered (sculpt/apply-layer-stroke with-layer (sculpt/brush [0 1 0] 0.8 0.15 :inflate) [:x])
        active (:sculpt/active-layer layered)
        hidden (sculpt/update-layer layered active assoc :sculpt.layer/visible? false)
        half (sculpt/update-layer layered active assoc :sculpt.layer/opacity 0.5)]
    (is (= (:positions base) (:positions (:sculpt/base layered))))
    (is (= 2 (count (:sculpt/layers layered))))
    (is (not= (:positions (sculpt/evaluate-document layered)) (:positions (sculpt/evaluate-document hidden))))
    (is (not= (:positions (sculpt/evaluate-document layered)) (:positions (sculpt/evaluate-document half))))
    (is (= 1 (count (:sculpt/layers (sculpt/delete-layer layered active)))))))

(deftest layered-mask-and-topology-rebase
  (let [doc (sculpt/sculpt-document (sculpt/sphere-mesh 1 8 4))
        masked (sculpt/apply-layer-stroke doc (sculpt/brush [1 0 0] 0.8 1 :mask) nil)
        rebased (sculpt/subdivide-document masked)]
    (is (some pos? (get-in masked [:sculpt/base :masks])))
    (is (= 1 (count (:sculpt/layers rebased))))
    (is (> (count (get-in rebased [:sculpt/base :positions]))
           (count (get-in doc [:sculpt/base :positions]))))
    (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
                 (sculpt/delete-layer doc 1)))))
