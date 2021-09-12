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

(def plate-thickness 4.5)
(def mount-thickness 2)
(def switch-clip-thickness 1.5)
(def columns (range 0 5))
(def rows (range 0 9))
(def row-number-offset 4)
(def left-to-right-curve (deg2rad 5))
(def front-to-back-curve (deg2rad 15))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  SINGLE SWITCH MOUNT   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def switch-mount
  (let [front-wall (->> (cube (+ keyswitch-depth (* mount-thickness 2)) mount-thickness plate-thickness)
        (translate [0
                    (/ (+ keyswitch-depth mount-thickness) 2)
                    ;(/ plate-thickness 2)]))
                    0]))
        side-wall (->> (cube mount-thickness (+ keyswitch-width (* mount-thickness 2)) plate-thickness)
                       (translate [(/ (+ keyswitch-width mount-thickness) 2)
                                   0
                                   ;(/ plate-thickness 2)]))
                                   0]))
        clip-hole (->> (cube clip-width clip-depth clip-height)
                       (translate [0
                                   (- (+ keyswitch-depth clip-depth) (/ (+ keyswitch-depth clip-depth) 2))
                                   ;(- (- plate-thickness switch-clip-thickness) (/ clip-height 2))]))
                                   (- (- (/ plate-thickness 2) switch-clip-thickness) (/ clip-height 2))]))
        half-mount (difference (union front-wall side-wall)
                               clip-hole)]
    (union half-mount
      (->> half-mount
           (mirror [1 0 0])
           (mirror [0 1 0])))))

;;;;;;;;;;;;;;;;;;;;
;;   SINGLE ROW   ;;
;;;;;;;;;;;;;;;;;;;;

;(def front-to-back-radius (Math/sin

(defn row-y-offset [row]
  (reduce + (for [x (range row)]
              (* (Math/cos (* front-to-back-curve x)) (+ keycap-depth (* mount-thickness 2)))
              )))

(defn row-z-offset [row]
  (- (reduce + (for [x (range row)]
              (* (Math/sin (* front-to-back-curve x)) (+ keycap-depth (* mount-thickness 2))))
              ) (/ (+ keycap-depth (* mount-thickness 2)) 2)))

(defn row-placement [row shape]
  (let [angle (* front-to-back-curve (- row row-number-offset))
        offset [0
                (* (row-y-offset (Math/abs (- row row-number-offset))) (cond (< (- row row-number-offset) 0) -1 :else 1))
                ;(* (* (Math/cos angle) (+ keycap-depth (* mount-thickness 2))) (- row row-number-offset))
                ;(Math/abs (* (* (Math/sin angle) (* (+ keycap-depth (* mount-thickness 2)) 0.5)) (- row row-number-offset)))]]
                (row-z-offset (Math/abs (- row row-number-offset)))]]
    (->> shape
         (rotate angle [1 0 0])
         (translate offset))))

(def key-holes
  (apply union
         (for [row rows]
           (->> switch-mount
                (row-placement row)))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;   SPIT or output   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(spit "things/start-a-dactyl-2.scad"
      (write-scad key-holes))
