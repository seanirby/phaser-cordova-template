;;; generates atlas files from an XCF spritesheet
;;; assumes each layer is a separate texture/sprite
;;; the layer boundary size is assumed to be the sprite size

(define (make-atlas-files inImage inBorderSize inString inDirName)

  ;; joins a list of strings using a separator
  (define (join-list separator lst)
    (foldr (lambda (x y) (string-append x separator y)) separator lst))

  ;; creates an attribute pair having the form 'attrName="attrValue"'
  (define (attr-pair attrName attrValue)
    (string-append attrName "=\"" attrValue "\""))

  ;; makes a single tag xml element
  (define (single-tag name attributes)
    (string-append "\n<" name " " attributes "/>\n"))

  ;; makes a double tag xml element
  (define (double-tag name attributes contents)
    (string-append "\n<" name " " attributes ">\n" contents "\n</" name ">\n"))

  (define (maybe-number->string x)
    (if (number? x)
        (number->string x)
        x))

  ;; determines texture attributes from layer position and boundaries
  ;; note: currently doesn't support custom frame sizes
  ;; todo: find a way to add and extract layer metadata for supporting framesizes
  (define (get-attributes-from-layer layer spritesheet)
    (gimp-image-set-active-layer spritesheet layer)
    (let* ((drawable (car (gimp-image-get-active-drawable spritesheet)))
           (name (car (gimp-layer-get-name layer)))
           (x (+ inBorderSize (car (gimp-drawable-offsets drawable))))
           (y (+ inBorderSize (cadr (gimp-drawable-offsets drawable))))
           (width (- (list-ref (gimp-drawable-mask-bounds drawable) 3) (* inBorderSize 2)))
           (height (- (list-ref (gimp-drawable-mask-bounds drawable) 4) (* inBorderSize 2)))
           (frameX 0)
           (frameY 0)
           (frameWidth width)
           (frameHeight height)
           (attr-pairs (map (lambda (x y)
                              (attr-pair x (maybe-number->string y)))
                            (list "name" "x" "y" "width" "height" "frameX" "frameY" "frameWidth" "frameHeight")
                            (list name   x  y  width   height   frameX   frameY   width   height))))
      ;; close image
      (join-list " " attr-pairs)))

  (define (make-subtexture-tags spritesheet)
    (join-list "\n"
               (map (lambda (layer)
                      (single-tag "SubTexture" (get-attributes-from-layer layer spritesheet)))
                    (vector->list (cadr (gimp-image-get-layers spritesheet))))))

  (let* ((output-folder inDirName)
         (spritesheet inImage)
         (atlas-xml-filename (string-append inString ".xml"))
         (atlas-image-filename (string-append inString ".png"))
         (xml-tag "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")
         (outport (open-output-file (string-append output-folder "/" atlas-xml-filename))))

    ;; save the atlas image
    (gimp-selection-all spritesheet)
    (gimp-edit-copy-visible spritesheet)
    (let ((img (car (gimp-edit-paste-as-new))))
      (gimp-file-save
       RUN-NONINTERACTIVE
       img
       (car (gimp-image-get-active-drawable img))
       (string-append output-folder "/" atlas-image-filename)
       (string-append output-folder "/" atlas-image-filename))
      (gimp-image-delete img))

    ;; generate atlas xml file from image layers
    (display
     (string-append
      xml-tag
      (double-tag "TextureAtlas"
                  (string-append " imagePath=\"" atlas-image-filename "\"")
                  (make-subtexture-tags spritesheet))) outport)
    (close-output-port outport)))

(script-fu-register "make-atlas-files"
                    "Make Atlas Files"
                    "Generates PNG and XML atlas files from the current image.  Each atlas texture is generated from the image's layers.  The texture name and texture size are taken from the layer name and layer boundaries."
                    "Sean Irby <sean.t.irby@gmail.com>"
                    "Sean Irby"
                    "2016-07-03"
                    ""
                    SF-IMAGE   "Image" 0
                    SF-ADJUSTMENT "Border Size" '(2 0 10 1 1 0 SF-SLIDER)
                    SF-STRING  "Output Filename" "atlas"
                    SF-DIRNAME "Output Directory" "./"
                    )

(script-fu-menu-register "make-atlas-files" "<Image>/Image")
