;;; This script is used for the to save layers of a spritesheet to individual pngs.
;;; Currently hardcoded for tone-master game;


(define (image-builder)
  ;; the layered spritesheet
  (define spritesheet-file "/home/sean/sync/dev/tone-master/asset_projects/gfx/main.xcf" )
  (define output-folder "/home/sean/sync/dev/tone-master/static/gfx")
  ;; the size of the border(pixels) surrounding each layer
  (define border 2)
  (let ((spritesheet (car (gimp-xcf-load 0 spritesheet-file spritesheet-file))))
    ;;iterate through layers
    ;;for each one->
    ;; -make active
    ;; -select by layer boundary size adjusted for border
    ;; -copy to temporary image
    ;; -export gfx folder using layer name as filename
    ;; -delete temporary image
    (map (lambda (layer)
           (gimp-image-set-active-layer spritesheet layer)
           (gimp-edit-copy layer)
           (let ((img (car (gimp-edit-paste-as-new))))
             (gimp-selection-all img)
             (gimp-selection-shrink img border)
             (let* ((bounds (gimp-selection-bounds img))
                    (x1 (list-ref bounds 1))
                    (x2 (list-ref bounds 3))
                    (y1 (list-ref bounds 2))
                    (y2 (list-ref bounds 4))
                    (width (- x2 x1))
                    (height (- y2 y1)))
               (gimp-image-crop img width height x1 y1)
               (gimp-file-save
                RUN-NONINTERACTIVE
                img
                (car (gimp-image-get-active-drawable img))
                (string-append output-folder "/" (car (gimp-layer-get-name layer)) ".png")
                (string-append output-folder "/" (car (gimp-layer-get-name layer)) ".png"))
               (gimp-image-delete img))))
         (vector->list (cadr (gimp-image-get-layers spritesheet))))))

(script-fu-register "image-builder"
                    "Image Builder"
                    "Builds many images from a spritesheet"
                    "Sean Irby <sean.t.irby@gmail.com>"
                    "Sean irby"
                    "2016-06-30"
                    ""
                    ;;SF-VALUE "size" "100"
                    ;;SF-COLOR "color" '(255 127 0)
                    )
(script-fu-menu-register "image-builder" "<Image>/Image")

