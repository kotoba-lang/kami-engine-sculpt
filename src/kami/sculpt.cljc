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
     {:positions positions :normals (mapv normalize positions) :indices indices})))

(defn apply-mesh-brush
  "Apply an actual brush displacement to mesh positions. Supported modes are
  :inflate, :smooth and :pinch. Returns a new immutable mesh."
  [mesh {:brush/keys [center radius strength mode]}]
  (let [avg-radius (/ (reduce + (map length (:positions mesh))) (count (:positions mesh)))
        deform (fn [p n]
                 (let [delta (sub p center) d (length delta)]
                   (if (>= d radius) p
                     (let [falloff (let [x (- 1 (/ d radius))] (* x x))
                           amount (* strength falloff)]
                       (case mode
                         :smooth (add p (scale (sub (scale (normalize p) avg-radius) p) amount))
                         :pinch (add p (scale (sub center p) (* amount 0.25)))
                         (add p (scale n amount)))))))]
    (let [positions (mapv deform (:positions mesh) (:normals mesh))]
      (assoc mesh :positions positions :normals (mapv normalize positions)))))

(defn apply-stroke
  "Apply a brush and optional axis symmetry as one deterministic stroke."
  [mesh b symmetry-axis]
  (cond-> (apply-mesh-brush mesh b)
    symmetry-axis (apply-mesh-brush (mirrored b symmetry-axis))))
