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
