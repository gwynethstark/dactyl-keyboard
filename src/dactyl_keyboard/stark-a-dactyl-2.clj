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
(def clip-depth 0.8)

(def plate-thickness 8.5)
(def web-thickness 8.5)
(def mount-thickness 2)
(def switch-clip-thickness 1.5)
(def switch-alignment-offset 0)
(def post-size 0.1)
(def columns (range 0 5))
(def rows (range 0 6))
(def row-number-offset 2)
(def left-to-right-curve (deg2rad 5))
(def front-to-back-curve (deg2rad 15))

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
           (->> half-cap
                (mirror [1 0 0])
                (mirror [0 1 0])))))
;;;;;;;;;;;;;;;;;;;;
;;   SINGLE ROW   ;;
;;;;;;;;;;;;;;;;;;;;

;comment this function calculates the radius of a circle based on size of sin(angle)/mount-size. Then places objects on the outside of the curve
(def row-radius (+ (/ (/ (+ mount-depth switch-alignment-offset) 2)
                      (Math/sin (/ front-to-back-curve 2)))
                   (+ plate-thickness (+ cap-to-mount keycap-height))))

(defn row-placement [row shape]
  (let [radius-offset-shape (->> shape
                                 (translate [0 0 (- row-radius)])
                                 (rotate (* front-to-back-curve (- row row-number-offset)) [1 0 0])
                                 (translate [0 0 row-radius]))]
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

;;;;;;;;;;;;;;;;;;;;;;;;
;;   WEB CONNECTORS   ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(def web-post (->> (cube post-size post-size web-thickness)
                   (translate [0
                               0
                               (- (/ plate-thickness 2) (/ web-thickness 2))])))

(def web-post-fl (color [240/255 0 0 1] (translate[(- (- (/ mount-width 2) (/ post-size 2))) (- (- (/ mount-depth 2) (/ post-size 2))) 0] web-post)))
(def web-post-fr (color [240/255 0 0 1] (translate[(+ (/ mount-width 2) (/ post-size 2)) (- (- (/ mount-depth 2) (/ post-size 2))) 0] web-post)))
(def web-post-bl (color [240/255 0 0 1] (translate[(- (- (/ mount-width 2) (/ post-size 2))) (+ (/ mount-depth 2) (/ post-size 2)) 0] web-post)))
(def web-post-br (color [240/255 0 0 1] (translate[(+ (/ mount-width 2) (/ post-size 2)) (+ (/ mount-depth 2) (/ post-size 2)) 0] web-post)))

(def connectors
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

;;;;;;;;;;;;;;;;;;;;;;
;;   WALLS/BOTTOM   ;;
;;;;;;;;;;;;;;;;;;;;;;

(def curved-bottom
  (let [block (->> (cube mount-width (* mount-depth 7) 50)
                   (translate [0
                               (* mount-depth (- row-number-offset 1.5))
                               (- 25 (/ plate-thickness 2))]))
        c-curve (->> (cylinder row-radius (+ mount-width 2))
                     (rotate (deg2rad 90) [0 1 0])
                     (translate[0
                                0
                                (- row-radius (/ plate-thickness 2))]))
        slice (->> (cube (- mount-width (* 2 mount-thickness)) (* mount-depth 8) 50)
                   (translate [0
                               (* mount-depth (- row-number-offset 1.5))
                                24]))]
 (difference block c-curve slice)))
;;;;;;;;;;;;;;;;;;;;;;;;;
;;   SPIT or output   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(spit "things/start-a-dactyl-2.scad"
      (write-scad (union key-holes connectors curved-bottom)))
