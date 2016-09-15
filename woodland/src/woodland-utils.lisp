(in-package :om)

; decompose the points of a bpc trajectory into a list of bpc with one point each 
(defmethod* unpack-3dc-to-list-of-3dc ( (self 3DC) )
  :icon '(347)
  :menuins '()
  :indoc '("bpc")
  :outdoc '("")
  :initvals '(nil)
  :doc "adapt SPAT 3dc for multiple sources"

  (loop for p in (point-list self) collect
        ; 
         (om-init-instance (make-instance '3DC :x-points (om-point-x p) :y-points (om-point-y p) :z-points (om-point-z p) :times 0))
    )

)

  ; (defmethod init-bpf-points ((self 3DC))
  ; (set-bpf-points self 
  ;                 :x (slot-value self 'x-points)
  ;                 :y (slot-value self 'y-points)
  ;                 :z (slot-value self 'z-points)
  ;                 :time (slot-value self 'times)
  ;                 :time-types (slot-value self 'time-types))
  ; (time-sequence-update-internal-times self)
  ; self)

(defmethod* point-pairs-mod ((self 3dc)) 
  :initvals '(nil)
  :indoc '("a 3dc")
  :icon 241 
  :doc "mod of SPAT point-pairs method: retruns the list of points in <self> as a list ((x1 y1 z1) (x2 y2 z2) ...)"
  (mat-trans (list (x-points self) (y-points self) (z-points self) )))