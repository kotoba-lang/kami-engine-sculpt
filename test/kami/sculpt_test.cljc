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

(deftest adjacency-mask-filters
  (let [mesh {:positions [[0 0 0] [1 0 0] [0 1 0] [2 0 0]] :normals [[0 0 1] [0 0 1] [0 0 1] [0 0 1]]
              :indices [0 1 2 1 3 2] :masks [1.0 0.0 0.0 0.0]}
        blurred (sculpt/filter-mask mesh :blur) grown (sculpt/filter-mask mesh :grow)
        shrunk (sculpt/filter-mask (assoc mesh :masks [1 1 1 0]) :shrink)
        sharpened (sculpt/filter-mask (assoc mesh :masks [0.6 0.4 0.4 0.4]) :sharpen)]
    (is (< 0 (nth (:masks blurred) 1) 1))
    (is (= [1.0 1.0 1.0 0.0] (:masks grown)))
    (is (= [1 0 0 0] (:masks shrunk)))
    (is (> (first (:masks sharpened)) 0.6))
    (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs js/Error) (sculpt/filter-mask mesh :unknown)))))

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

(deftest layer-stack-operations-preserve-data
  (let [doc (-> (sculpt/sculpt-document (sculpt/sphere-mesh 1 8 4))
                (sculpt/apply-layer-stroke (sculpt/brush [1 0 0] 0.8 0.2 :inflate) nil)
                (sculpt/add-layer "Detail"))
        duplicate (sculpt/duplicate-layer doc 1) copy-id (:sculpt/active-layer duplicate)
        moved (sculpt/move-layer duplicate copy-id 0)
        before (:positions (sculpt/evaluate-document doc)) baked (sculpt/bake-layer doc 1)]
    (is (= 3 (count (:sculpt/layers duplicate))))
    (is (= copy-id (:sculpt.layer/id (first (:sculpt/layers moved)))))
    (is (= before (:positions (sculpt/evaluate-document baked))))
    (is (every? #(= [0.0 0.0 0.0] %) (:sculpt.layer/deltas (sculpt/find-layer baked 1))))))

(deftest deterministic-voxel-remesh
  (let [mesh {:positions [[0 0 0] [0.01 0 0] [1 0 0] [0 1 0] [0 1.01 0]]
              :normals (vec (repeat 5 [0 0 1])) :masks [0 1 0 0 1]
              :indices [0 2 3, 1 2 4, 0 1 2]}
        remeshed (sculpt/voxel-remesh mesh 0.1)]
    (is (= 3 (count (:positions remeshed))))
    (is (= 3 (count (:indices remeshed))))
    (is (= 1 (get-in remeshed [:remesh :result-triangles])))
    (is (== 0.5 (first (:masks remeshed))))
    (is (= remeshed (sculpt/voxel-remesh mesh 0.1)))
    (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs js/Error) (sculpt/voxel-remesh mesh 0)))))

(deftest voxel-remesh-rebases-sculpt-layers
  (let [doc (-> (sculpt/sculpt-document (sculpt/sphere-mesh 1 12 6))
                (sculpt/apply-layer-stroke (sculpt/brush [1 0 0] 0.8 0.3 :inflate) nil)
                (sculpt/add-layer "Detail"))
        remeshed (sculpt/remesh-document doc 0.25)
        vertex-count (count (get-in remeshed [:sculpt/base :positions]))]
    (is (= 1 (count (:sculpt/layers remeshed))))
    (is (= vertex-count (count (:sculpt.layer/deltas (first (:sculpt/layers remeshed))))))
    (is (pos? vertex-count))))

(deftest topology-diagnostics-for-retopology
  (let [open {:positions [[0 0 0] [1 0 0] [0 1 0] [2 2 2]] :normals (vec (repeat 4 [0 0 1])) :indices [0 1 2]}
        report (sculpt/topology-diagnostics open)
        closed (sculpt/topology-diagnostics (sculpt/sphere-mesh 1 8 4))]
    (is (= 3 (count (:topology/boundary-edges report))))
    (is (= [3] (:topology/isolated-vertices report)))
    (is (false? (:topology/manifold? report)))
    (is (false? (:topology/closed? report)))
    (is (:topology/manifold? closed))))

(deftest detects-degenerate-and-non-manifold-faces
  (let [mesh {:positions [[0 0 0] [1 0 0] [0 1 0] [0 -1 0] [0 0 1]] :normals (vec (repeat 5 [0 0 1]))
              :indices [0 1 2, 1 0 3, 0 1 4, 0 0 2]}
        report (sculpt/topology-diagnostics mesh)]
    (is (= [[0 1]] (:topology/non-manifold-edges report)))
    (is (= [3] (:topology/degenerate-faces report)))
    (is (false? (:topology/manifold? report)))))

(deftest deterministic-topology-repair
  (let [mesh {:positions [[0 0 0] [1 0 0] [0 1 0] [0 -1 0] [0 0 1] [9 9 9]]
              :normals (vec (repeat 6 [0 0 1])) :masks [0 0.1 0.2 0.3 0.4 1]
              :indices [0 1 2, 0 1 2, 1 0 3, 0 1 4, 0 0 2, 0 1 99]}
        repaired (sculpt/repair-topology mesh) report (sculpt/topology-diagnostics repaired)]
    (is (= 2 (get-in repaired [:repair :result-triangles])))
    (is (= 4 (get-in repaired [:repair :removed-triangles])))
    (is (empty? (:topology/non-manifold-edges report)))
    (is (empty? (:topology/degenerate-faces report)))
    (is (empty? (:topology/isolated-vertices report)))
    (is (= repaired (sculpt/repair-topology mesh)))))

(deftest topology-repair-rebases-layers
  (let [doc (-> (sculpt/sculpt-document (sculpt/sphere-mesh 1 8 4)) (sculpt/add-layer "Detail"))
        repaired (sculpt/repair-document doc)]
    (is (= 1 (count (:sculpt/layers repaired))))
    (is (= (count (get-in repaired [:sculpt/base :positions]))
           (count (:sculpt.layer/deltas (first (:sculpt/layers repaired))))))))
