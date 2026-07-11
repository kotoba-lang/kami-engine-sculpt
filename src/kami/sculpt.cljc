(ns kami.sculpt "Portable signed-distance-field sculpting operations.")
(defn sphere [center radius] {:sdf/kind :sphere :sdf/center center :sdf/radius radius})
(defn brush [center radius strength mode] {:brush/center center :brush/radius radius :brush/strength strength :brush/mode mode})
(defn apply-brush
  "Append a deterministic non-destructive brush operation. A mesher consumes this
  EDN stack to remesh; UI never mutates a binary mesh directly."
  [shape b] (update shape :sdf/operations (fnil conj []) b))
(defn mirrored [b axis]
  (let [[x y z] (:brush/center b) p {:x [(- x) y z] :y [x (- y) z] :z [x y (- z)]}]
    (assoc b :brush/center (get p axis))))

(defn- sin [x] #?(:clj (Math/sin x) :cljs (js/Math.sin x)))
(defn- cos [x] #?(:clj (Math/cos x) :cljs (js/Math.cos x)))
(defn- sqrt [x] #?(:clj (Math/sqrt x) :cljs (js/Math.sqrt x)))
(def pi #?(:clj Math/PI :cljs js/Math.PI))
(defn- sub [a b] (mapv - a b))
(defn- add [a b] (mapv + a b))
(defn- scale [v s] (mapv #(* % s) v))
(defn- mix [a b t] (add a (scale (sub b a) t)))
(defn- length [v] (sqrt (reduce + (map #(* % %) v))))
(defn- normalize [v] (let [l (max 1.0e-9 (length v))] (scale v (/ 1 l))))

(defn sphere-mesh
  "Generate a UV sphere suitable for deterministic brush deformation."
  ([radius] (sphere-mesh radius 32 20))
  ([radius slices stacks]
   (let [positions (vec (for [j (range (inc stacks)) i (range (inc slices))
                              :let [v (/ j stacks) u (/ i slices)
                                    phi (* pi v) theta (* 2 pi u)]]
                          [(* radius (sin phi) (cos theta))
                           (* radius (cos phi))
                           (* radius (sin phi) (sin theta))]))
         cols (inc slices)
         indices (vec (mapcat (fn [[j i]]
                                (let [a (+ (* j cols) i) b (inc a) c (+ a cols) d (inc c)]
                                  [a c b b c d]))
                              (for [j (range stacks) i (range slices)] [j i])))]
     {:positions positions :normals (mapv normalize positions) :indices indices
      :masks (vec (repeat (count positions) 0.0))})))

(defn clear-mask [mesh] (assoc mesh :masks (vec (repeat (count (:positions mesh)) 0.0))))
(defn invert-mask [mesh] (update mesh :masks #(mapv (fn [v] (- 1.0 v)) (or % (repeat (count (:positions mesh)) 0.0)))))

(defn apply-mask-brush
  "Paint or erase a per-vertex mask using the same quadratic brush falloff."
  [mesh {:brush/keys [center radius strength mode]}]
  (let [masks (or (:masks mesh) (vec (repeat (count (:positions mesh)) 0.0)))
        paint (fn [p current]
                (let [d (length (sub p center))]
                  (if (>= d radius) current
                    (let [x (- 1 (/ d radius)) amount (* strength x x)
                          value (if (= mode :mask-erase) (- current amount) (+ current amount))]
                      (max 0.0 (min 1.0 value))))))]
    (assoc mesh :masks (mapv paint (:positions mesh) masks))))

(defn apply-mesh-brush
  "Apply an actual brush displacement to mesh positions. Supported modes are
  :inflate, :smooth and :pinch. Returns a new immutable mesh."
  [mesh {:brush/keys [center radius strength mode] :as b}]
  (if (#{:mask :mask-erase} mode)
    (apply-mask-brush mesh b)
  (let [avg-radius (/ (reduce + (map length (:positions mesh))) (count (:positions mesh)))
        masks (or (:masks mesh) (repeat (count (:positions mesh)) 0.0))
        deform (fn [p n mask]
                 (let [delta (sub p center) d (length delta)]
                   (if (>= d radius) p
                     (let [falloff (let [x (- 1 (/ d radius))] (* x x))
                           amount (* strength falloff (- 1 mask))]
                       (case mode
                         :smooth (add p (scale (sub (scale (normalize p) avg-radius) p) amount))
                         :pinch (add p (scale (sub center p) (* amount 0.25)))
                         (add p (scale n amount)))))))]
    (let [positions (mapv deform (:positions mesh) (:normals mesh) masks)]
      (assoc mesh :positions positions :normals (mapv normalize positions))))))

(defn symmetry-brushes
  "Return unique brush instances for any combination of enabled symmetry axes."
  [b axes]
  (let [axes (cond (nil? axes) [] (keyword? axes) [axes] :else axes)]
    (reduce (fn [brushes axis]
              (vec (distinct (concat brushes (map #(mirrored % axis) brushes))))) [b] axes)))

(defn apply-stroke
  "Apply a brush and optional axis symmetry as one deterministic stroke."
  [mesh b symmetry-axis]
  (reduce apply-mesh-brush mesh (symmetry-brushes b symmetry-axis)))

(defn resample-stroke
  "Resample pointer input at stable world-space spacing for device-independent strokes."
  [points spacing]
  (when-not (pos? spacing) (throw (ex-info "stroke spacing must be positive" {:spacing spacing})))
  (if (< (count points) 2) (vec points)
    (loop [result [(first points)] remaining (rest points) cursor (first points)]
      (if-let [target (first remaining)]
        (let [delta (sub target cursor) d (length delta)]
          (if (>= d spacing)
            (let [next-point (add cursor (scale delta (/ spacing d)))]
              (recur (conj result next-point) remaining next-point))
            (recur result (rest remaining) target)))
        (cond-> result (> (length (sub (peek result) (last points))) 1.0e-9) (conj (last points)))))))

(defn subdivide-mesh
  "Uniformly subdivide indexed triangles while sharing edge midpoints. This is
  the deterministic topology step used by dynamic-topology and multires tools."
  [mesh]
  (let [positions (atom (vec (:positions mesh)))
        masks (atom (vec (or (:masks mesh) (repeat (count @positions) 0.0))))
        edge-cache (atom {})
        midpoint (fn [a b]
                   (let [edge (if (< a b) [a b] [b a])]
                     (if-let [i (get @edge-cache edge)] i
                       (let [i (count @positions)]
                         (swap! positions conj (mix (nth @positions a) (nth @positions b) 0.5))
                         (swap! masks conj (/ (+ (nth @masks a) (nth @masks b)) 2))
                         (swap! edge-cache assoc edge i) i))))
        triangles (partition 3 (:indices mesh))
        indices (vec (mapcat (fn [[a b c]]
                               (let [ab (midpoint a b) bc (midpoint b c) ca (midpoint c a)]
                                 [a ab ca, ab b bc, ca bc c, ab bc ca])) triangles))]
    (assoc mesh :positions @positions :normals (mapv normalize @positions) :masks @masks :indices indices)))

;; Non-destructive sculpt layers. Each layer stores additive position deltas
;; against a stable base topology; evaluation is deterministic and portable.
(defn sculpt-layer
  ([id name vertex-count] (sculpt-layer id name vertex-count {}))
  ([id name vertex-count {:keys [opacity visible?] :or {opacity 1.0 visible? true}}]
   {:sculpt.layer/id id :sculpt.layer/name name :sculpt.layer/opacity opacity
    :sculpt.layer/visible? visible? :sculpt.layer/deltas (vec (repeat vertex-count [0.0 0.0 0.0]))}))
(defn sculpt-document [base-mesh]
  (let [layer (sculpt-layer 1 "Base detail" (count (:positions base-mesh)))]
    {:sculpt/base base-mesh :sculpt/layers [layer] :sculpt/active-layer 1 :sculpt/next-layer-id 2}))
(defn find-layer [doc id] (first (filter #(= id (:sculpt.layer/id %)) (:sculpt/layers doc))))
(defn evaluate-document [doc]
  (let [base (:sculpt/base doc)
        deltas (filter :sculpt.layer/visible? (:sculpt/layers doc))
        positions (mapv (fn [index p]
                          (reduce (fn [result layer]
                                    (add result (scale (nth (:sculpt.layer/deltas layer) index)
                                                       (:sculpt.layer/opacity layer)))) p deltas))
                        (range) (:positions base))]
    (assoc base :positions positions :normals (mapv normalize positions))))
(defn add-layer [doc name]
  (let [id (:sculpt/next-layer-id doc) layer (sculpt-layer id name (count (get-in doc [:sculpt/base :positions])))]
    (-> doc (update :sculpt/layers conj layer) (assoc :sculpt/active-layer id) (update :sculpt/next-layer-id inc))))
(defn update-layer [doc id f & args]
  (update doc :sculpt/layers #(mapv (fn [layer] (if (= id (:sculpt.layer/id layer)) (apply f layer args) layer)) %)))
(defn delete-layer [doc id]
  (when (= 1 (count (:sculpt/layers doc))) (throw (ex-info "sculpt document needs one layer" {})))
  (let [layers (vec (remove #(= id (:sculpt.layer/id %)) (:sculpt/layers doc)))]
    (assoc doc :sculpt/layers layers :sculpt/active-layer
           (if (= id (:sculpt/active-layer doc)) (:sculpt.layer/id (first layers)) (:sculpt/active-layer doc)))))
(defn apply-layer-stroke [doc b symmetry]
  (let [before (evaluate-document doc) after (apply-stroke before b symmetry)
        increment (mapv sub (:positions after) (:positions before)) active (:sculpt/active-layer doc)]
    (if (#{:mask :mask-erase} (:brush/mode b))
      (assoc-in doc [:sculpt/base :masks] (:masks after))
      (update-layer doc active update :sculpt.layer/deltas #(mapv add % increment)))))
(defn subdivide-document
  "Bake visible layers, subdivide topology, and start a fresh detail layer."
  [doc]
  (sculpt-document (subdivide-mesh (evaluate-document doc))))
