(ns dactyl-keyboard.stark-a-dactyl-2
  (:refer-clojure :exclude [use import])
  (:require [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [dactyl-keyboard.util :refer :all]
            [unicode-math.core :refer :all]))

(defn deg2rad [degrees]
  (* (/ degrees 180) pi))

(def keyswitch-width 13.8) ;;keyswithc width left to right measured on kailh box jades
(def total-keyswitch-depth 15.55) ;;keyswitch depth front to back
(def keyswitch-depth 13.9) ;;keyswitch depth where it slots into a plate.
(def extrusion-diameter 3.8)

(def total-switch-height 18.2) ;;total height of keyswitch step to prong without keycap
(def total-switch-height-with-keycap 23.1)
(def switch-height-without-stem 14.6)
(def switch-height-housing+extrusion 12.8)
(def switch-housing-height 11.2)

(def stem-height (- total-switch-height switch-height-without-stem))
(def prong-length (- switch-height-without-stem switch-housing-height))
(def extrusion-height (- switch-height-housing+extrusion switch-housing-height))

(def keycap-depth 18)
(def keycap-width 18.1)
(def keycap-height 9.4)
(def keycap-height-additon (- total-switch-height-with-keycap total-switch-height))

(def clip-width 4.75)
(def clip-height 1.5)
(def clip-depth 1.0)

(def wall-width 4.5)
(def plate-thickness 4.5)
(def web-thickness 3.5)
(def mount-thickness 2)
(def switch-clip-thickness 1.5)
(def switch-alignment-row-offset -2)
(def switch-alignment-column-offset 1.2)
(def post-size 0.1)
(def columns (range 0 6))
(def rows (range 0 4))
(def row-number-offset 1)
(def column-number-offset 0)
(def left-to-right-curve (deg2rad 10))
(def front-to-back-curve (deg2rad 15))
(def finger-height-offset {:pinky 6.9
                           :ringfinger 1.7
                           :middlefinger -0.5
                           :indexfinger 1.7})
(def finger-depth-offset {:pinky -10.8
                          :ringfinger 0
                          :middlefinger 2.4
                          :indexfinger 0})

(def additional-outer-column-curve (deg2rad 45))
(def wing-tip-y-offsets [1.4 1.3 1.3 1.2])

(def mount-width (+ keyswitch-width (* mount-thickness 2)))
(def mount-depth (+ keyswitch-depth (* mount-thickness 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  SINGLE SWITCH MOUNT   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def switch-mount
  (let [front-wall (->> (cube mount-depth mount-thickness plate-thickness)
                        (translate [0
                                    (/ (+ keyswitch-depth mount-thickness) 2)
                                    0]))
        side-wall (->> (cube mount-thickness mount-width plate-thickness)
                       (translate [(/ (+ keyswitch-width mount-thickness) 2)
                                   0
                                   0]))
        clip-hole (->> (cube clip-width clip-depth clip-height)
                       (translate [0
                                   (- (+ keyswitch-depth clip-depth) (/ (+ keyswitch-depth clip-depth) 2))
                                   (- (- (/ plate-thickness 2) switch-clip-thickness) (/ clip-height 2))]))
        half-mount (difference (union front-wall side-wall)
                               clip-hole)]
    (union half-mount
      (->> half-mount
           (mirror [1 0 0])
           (mirror [0 1 0])))))

;;;;;;;;;;;;;;;;
;;   KEYCAP   ;;
;;;;;;;;;;;;;;;;

(def cap-to-mount 5.4)
(def keycap-thickness 1.6)

(def keycap
  (let [front-wall (->> (cube mount-depth keycap-thickness keycap-height)
                        (translate [0
                                    (/ (+ keyswitch-depth mount-thickness) 2)
                                    (+ (+ cap-to-mount (/ plate-thickness 2)) (/ keycap-height 2))]))
        side-wall (->> (cube keycap-thickness mount-width keycap-height)
                       (translate [(/ (+ keyswitch-depth mount-thickness) 2)
                                   0
                                   (+ (+ cap-to-mount (/ plate-thickness 2)) (/ keycap-height 2))]))
        half-cap (union front-wall side-wall)]
    (union half-cap
           (color [240/255 233/255 175/255 1])
           (->> half-cap
                (mirror [1 0 0])
                (mirror [0 1 0])
                (color [240/255 233/255 175/255 1])))))

;;;;;;;;;;;;;;;;;;;;
;;   SINGLE ROW   ;;
;;;;;;;;;;;;;;;;;;;;

;comment this function calculates the radius of a circle based on size of sin(angle)/mount-size. Then places objects on the outside of the curve
(def row-radius (+ (/ (/ (+ mount-depth switch-alignment-row-offset) 2)
                      (Math/sin (/ front-to-back-curve 2)))
                   (+ plate-thickness (+ cap-to-mount keycap-height))))

(defn row-placement [row shape]
  (let [radius-offset-shape (->> shape
                                 (translate [0 0 (- row-radius)])
                                 (rotate (* front-to-back-curve (- row row-number-offset)) [1 0 0])
                                 (translate [0 0 row-radius])
                             )
                            ]

      radius-offset-shape))

(def key-holes
  (apply union
         (for [row rows]
           (->> switch-mount
                (row-placement row)))))

(def keycaps
  (apply union
         (for [row rows]
           (->> keycap
                (row-placement row)))))

;;;;;;;;;;;;;;;;;
;;   COLUMNS   ;;
;;;;;;;;;;;;;;;;;

(def column-radius (+ (/ (/ (+ mount-width switch-alignment-column-offset) 2)
                         (Math/sin (/ left-to-right-curve 2)))
                      (+ plate-thickness (+ cap-to-mount keycap-height))))

(defn column-offset [column shape]
  (let [height-offset (cond
                        (= column 0) (get finger-height-offset :pinky)
                        (= column 1) (get finger-height-offset :pinky)
                        (= column 2) (get finger-height-offset :ringfinger)
                        (= column 3) (get finger-height-offset :middlefinger)
                        (= column 4) (get finger-height-offset :indexfinger)
                        (= column 5) (get finger-height-offset :indexfinger)
                        :else 1.7
                        )
        depth-offset (cond
                       (= column 0) (get finger-depth-offset :pinky)
                       (= column 1) (get finger-depth-offset :pinky)
                       (= column 2) (get finger-depth-offset :ringfinger)
                       (= column 3) (get finger-depth-offset :middlefinger)
                       (= column 4) (get finger-depth-offset :indexfinger)
                       (= column 5) (get finger-depth-offset :indexfinger)
                       :else 1.7
                       )
        offset-shape (->> shape
                            (translate [0
                                        depth-offset
                                        height-offset
                                        ]))]
    offset-shape))

(defn column-placement [column shape]
  (let [radius-offset-shape (->> shape
                                 (translate [0 0 (- column-radius)])
                                 (rotate (* left-to-right-curve (- column column-number-offset)) [0 -1 0])
                                 (translate [0 0 column-radius]))
        offset-shape (column-offset (- column column-number-offset) radius-offset-shape)]
  offset-shape))

(def column-shapes
  (apply union
         (for [column columns]
           (->> key-holes
                (column-placement column)))))

(def keycaps-columns
  (apply union
         (for [column columns]
           (->> keycaps
                (column-placement column)))))

;;;;;;;;;;;;;;;;;;;;;;;
;;   OUTER-COLUMNS   ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defn row-radius-no-offset [n] (+ (/ (/ (- mount-depth (* 4 n)) 2)
                      (Math/sin (/ front-to-back-curve 2)))
                   (+ plate-thickness (+ cap-to-mount keycap-height))))


(defn row-offset-no-angle [row shape n]
  (let [radius-offset-shape (->> shape
                                 (rotate (* front-to-back-curve (- row row-number-offset)) [-1 0 0])
                                 (translate [0 0 (- (row-radius-no-offset n))])
                                 (rotate (* front-to-back-curve (- row row-number-offset)) [1 0 0])
                                 (translate [0 0 (row-radius-no-offset n)]))]
      radius-offset-shape))

(defn rotate-shape-to-column [row shape]
  (let [rotated-shape (->> shape
                           (rotate additional-outer-column-curve [0 1 0])
                           (translate [0
                                       0
                                       (* (Math/abs (- row row-number-offset)) (Math/sin front-to-back-curve))]
                                      ))]
    rotated-shape
    ))


(defn left-outer-column-rotation [shape]
    (->> (column-placement 1 shape)
         (translate[(- (- (+ mount-width (* (Math/cos additional-outer-column-curve) total-switch-height-with-keycap)) 3))
                    0
                   15])))

(def offset-row
  (union (row-offset-no-angle 0 (rotate-shape-to-column 0 switch-mount) 1.0)
         (row-offset-no-angle 1 (rotate-shape-to-column 1 switch-mount) 1.3)
         (row-offset-no-angle 2 (rotate-shape-to-column 2 switch-mount) 1.3)
         (row-offset-no-angle 3 (rotate-shape-to-column 3 switch-mount) 1.2)))

(def left-outer-column
  (left-outer-column-rotation offset-row))

(def outer-column-keycaps
  (union (row-offset-no-angle 0 (rotate-shape-to-column 0 keycap) 1.0)
         (row-offset-no-angle 1 (rotate-shape-to-column 1 keycap) 1.3)
         (row-offset-no-angle 2 (rotate-shape-to-column 2 keycap) 1.3)
         (row-offset-no-angle 3 (rotate-shape-to-column 3 keycap) 1.2)))

(def outer-keycaps
  (left-outer-column-rotation outer-column-keycaps))

;;;;;;;;;;;;;;;;;;;;;;;;
;;   WEB CONNECTORS   ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(def web-post (->> (cube post-size post-size web-thickness)
                   (translate [0
                               0
                               (- (- (/ plate-thickness 2) (/ web-thickness 2)) 0.4)])))

(def web-post-fl (color [240/255 0 0 1] (translate[(- (- (/ mount-width 2) (/ post-size 2))) (- (- (/ mount-depth 2) (/ post-size 2))) 0] web-post)))
(def web-post-fr (color [240/255 0 0 1] (translate[(+ (/ mount-width 2) (/ post-size 2)) (- (- (/ mount-depth 2) (/ post-size 2))) 0] web-post)))
(def web-post-bl (color [240/255 0 0 1] (translate[(- (- (/ mount-depth 2) (/ post-size 2))) (+ (/ mount-width 2) (/ post-size 2)) 0] web-post)))
(def web-post-br (color [240/255 0 0 1] (translate[(+ (/ mount-depth 2) (/ post-size 2)) (+ (/ mount-width 2) (/ post-size 2)) 0] web-post)))

(def row-hulls
  (apply union
         (concat
           (for [row (drop-last rows)]
             (triangle-hulls
               (row-placement (inc row) web-post-fl)
               (row-placement (inc row) web-post-fr)
               (row-placement row web-post-bl)
               (row-placement row web-post-br))
             )
           )))

(def column-hulls
  (apply union
         (concat
           (for [column (drop-last columns)]
             (for [row rows]
                 (triangle-hulls
                   (column-placement (inc column) (row-placement row web-post-fl))
                   (column-placement column (row-placement row web-post-fr))
                   (column-placement (inc column) (row-placement row web-post-bl))
                   (column-placement column (row-placement row web-post-br))))))))

(def diagonal-hulls
  (apply union
         (concat
           (for [column (drop-last columns)]
            (for [row (drop-last rows)]
             (triangle-hulls
              (column-placement (inc column) (row-placement (inc row) web-post-fl))
              (column-placement column (row-placement (inc row) web-post-fr))
              (column-placement (inc column) (row-placement row web-post-bl))
              (column-placement column (row-placement row web-post-br))))))))

(def column-connectors
  (apply union
         (for [column columns]
           (->> row-hulls
                (column-placement column)))))

(def column-row-connectors
  (apply union
         (for [column columns]
           (->> column-hulls
                (column-placement column)))))

(def outer-row-hulls
  (->> (left-outer-column-rotation (apply union
         (concat
           (for [row (drop-last rows)]
             (triangle-hulls
               (row-offset-no-angle (inc row) (rotate-shape-to-column (inc row)  web-post-fl) (cond (= row 0) 1.3 (= row 1) 1.3 (= row 2) 1.2 (= row 3) 1.2))
               (row-offset-no-angle (inc row) (rotate-shape-to-column (inc row) web-post-fr) (cond (= row 0) 1.3 (= row 1) 1.3 (= row 2) 1.2 (= row 3) 1.2))
               (row-offset-no-angle row (rotate-shape-to-column row web-post-bl) (cond (= row 0) 1.2 (= row 1) 1.3 (= row 2) 1.3 (= row 3) 1.2 ))
               (row-offset-no-angle row (rotate-shape-to-column row web-post-br) (cond (= row 0) 1.2 (= row 1) 1.3 (= row 2) 1.3 (= row 3) 1.2 )))
             )
           )))))

(def outer-column-hulls
  (apply union
         (concat
           (for [row rows]
             (triangle-hulls
               (column-placement 1 (row-placement row web-post-fl))
               (left-outer-column-rotation (row-offset-no-angle row (rotate-shape-to-column row web-post-fr) (cond (= row 0) 1.2 (= row 1) 1.3 (= row 2) 1.3 (= row 3) 1.2)))
               (column-placement 1 (row-placement row web-post-bl))
               (left-outer-column-rotation (row-offset-no-angle row (rotate-shape-to-column row web-post-br) (cond (= row 0) 1.2 (= row 1) 1.3 (= row 2) 1.3 (= row 3) 1.2)))
             )))))

(def outer-diagonal-hulls
  (apply union
         (concat
           (for [row (drop-last rows)]
             (triangle-hulls
               (column-placement 1 (row-placement (inc row) web-post-fl))
               (left-outer-column-rotation (row-offset-no-angle (inc row) (rotate-shape-to-column (inc row) web-post-fr) (cond (= row 0) 1.3 (= row 1) 1.3 (= row 2) 1.2 (= row 3) 1.2)))
               (column-placement 1 (row-placement row web-post-bl))
               (left-outer-column-rotation (row-offset-no-angle row (rotate-shape-to-column row web-post-br) (cond (= row 0) 1.2 (= row 1) 1.3 (= row 2) 1.3 (= row 3) 1.2)))
             )))))


(def outer-hulls
  (union outer-row-hulls outer-column-hulls outer-diagonal-hulls))


;;;;;;;;;;;;;;;;;;;;;;;
;;   THUMB CLUSTER   ;;
;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;
;;   WALLS/BOTTOM   ;;
;;;;;;;;;;;;;;;;;;;;;;

(def curved-bottom
  (let [block (->> (cube (* mount-width 6.6) (* (+ mount-depth switch-alignment-row-offset) 5) 50)
                   (translate [(* mount-width 1.6)
                               (* mount-depth (- row-number-offset 0.5))
                               (- 20 (/ plate-thickness 2))]))
        c-curve (->> (cylinder row-radius (+ mount-width 15))
                     (rotate (deg2rad 90) [0 1 0])
                     (translate[0
                                0
                                (- row-radius (/ plate-thickness 2))]))
        slice (->> (cube (- (* mount-width 5.6) (* 2 mount-thickness)) (* mount-depth 8) 59)
                   (translate [(* mount-width 1.1)
                               (* mount-depth (- row-number-offset 1.5))
                                20]))
        slice-2 (->> (cube (- mount-width mount-thickness) (* mount-depth 8) 50)
                     (translate [(/ mount-thickness 2)
                                 (* mount-depth (- row-number-offset 1.5))
                                 15]))]
 (difference block (column-placement 0 c-curve) (column-placement 5 c-curve) slice)))

(def curved-bottom-right
  (let [block (->> (cube mount-width (* (+ mount-depth switch-alignment-row-offset) 5) 50)
                   (translate [0
                               (* mount-depth (- row-number-offset 0.5))
                               (- 20 (/ plate-thickness 2))]))
        c-curve (->> (cylinder row-radius (+ mount-width 2))
                     (rotate (deg2rad 90) [0 1 0])
                     (translate[0
                                0
                                (- row-radius (/ plate-thickness 2))]))
        slice (->> (cube (- mount-width (* 2 mount-thickness)) (* mount-depth 8) 50)
                   (translate [0
                               (* mount-depth (- row-number-offset 1.5))
                                23]))
        slice-2 (->> (cube (- mount-width mount-thickness) (* mount-depth 8) 50)
                     (translate [(- (/ mount-thickness 2))
                                 (* mount-depth (- row-number-offset 1.5))
                                 15]))]
 (difference block c-curve slice-2)))

(def left-wall
  (let [block (->> (cube wall-width (* (+ mount-depth switch-alignment-row-offset) 4.8) 50)
                   (translate[(* mount-width (- row-number-offset 1.55))
                              (* mount-depth (- row-number-offset 1.1))
                              20]))
        c-curve (->> (cylinder (- row-radius 8) (+ mount-width 15))
                     (rotate (deg2rad 90) [0 1 0])
                     (translate[0
                                (+ (get finger-depth-offset :pinky) 1.6)
                                (- row-radius (/ plate-thickness 2))]))
        ]
    (difference block c-curve))
  )

(def right-wall
 (let [angle (* left-to-right-curve 4.5)
       block (->> (cube wall-width (* (+ mount-depth switch-alignment-row-offset) 4.8) 120)
                  (rotate angle [0 -1 0])
                  (translate[(* mount-width 7.5)
                             (+ (* mount-depth (- row-number-offset 0.5)) (get finger-depth-offset :indexfinger))
                            25]))
       c-curve (->> (cylinder row-radius (+ mount-width 5))
                    (translate [-54
                                0
                                0])
                    (rotate angle [0 1 0])
                    (translate[(* mount-width 5)
                               (get finger-depth-offset :indexfinger)
                               (- row-radius (/ plate-thickness 2))]))
       block2 (->> (cube wall-width (* (+ mount-depth switch-alignment-row-offset) 4.8) 70)
                   (translate[(* mount-width 6.2)
                              (+ (* mount-depth (- row-number-offset 0.5)) (get finger-depth-offset :indexfinger))
                              13]))
      subblock (->> (cube 100 (* (+ mount-depth switch-alignment-row-offset) 4.8) 100)
                    (translate [(* mount-width 9.14)
                                (+ (* mount-depth (- row-number-offset 0.5)) (get finger-depth-offset :indexfinger))
                                13])
                    (color [254/255 0 0 1]))

        ]
   (union (difference block c-curve subblock) block2 ))
  )
;;;;;;;;;;;;;;;;;;;;;;;;;
;;   SPIT or output   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(def fingerboard
  (->> (union column-shapes column-connectors column-hulls diagonal-hulls left-wall right-wall)
       (mirror [1 0 0])))

(spit "things/start-a-dactyl-2.scad"
      (write-scad fingerboard))

(spit "things/single-mount.scad"
      (write-scad switch-mount))
