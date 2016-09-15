(in-package :om)

; load foreign library
(cffi::use-foreign-library libomwoodland)

;;;======================================================================

; STRUCT

; OM-SOUND C STRUCT
(cffi:defcstruct audiobuffer-struct
  ( numChannels :unsigned-int )
  ( numSamples  :unsigned-int )
  ; ( interleaved :boolean )
  ( data 		:pointer )
 )

(cffi::defctype audiobuffer-type (:pointer (:struct audiobuffer-struct)))

; LISP-LIST C STRUCT
(cffi:defcstruct list-struct
  ( size :unsigned-int )
  ( data :pointer )
)

(cffi::defctype list-type (:pointer (:struct list-struct)))

; LISP-LIST-OF-LIST C STRUCT
(cffi:defcstruct list-of-list-struct
  ( size1 :unsigned-int )
  ( size2 :unsigned-int )
  ( data :pointer )
)

(cffi::defctype list-of-list-type (:pointer (:struct list-of-list-struct)))

;;;======================================================================

; MAIN

(cffi:defcfun  ("OmWoodlandRegisterTopology" register-topology-cfun) :void ; return void
  ( posnodes (:pointer (:struct list-struct) ) )
)

(cffi:defcfun  ("OmWoodlandSimulatePropagation" simulate-propagation-cfun) :void ; return void
  ( emitter-id :unsigned-int )
)

(cffi:defcfun  ("OmWoodlandSetSampleRate" set-sample-rate-cfun) :void ; return void
  ( sample-rate :unsigned-int )
)

(cffi:defcfun  ("OmWoodlandSetSpeedGainRxThreshold" set-speed-gain-rxThresh-cfun) :void ; return void
  ( speed :float )
  ( gain :float )
  ( rx-threshold :float )
)



(cffi:defcfun  ("OmWoodlandGetMaxPropagationTime" get-max-propagation-time-cfun) :float) ; return float

(cffi:defcfun  ("OmWoodlandConvolveInputToOutput" convolve-input-to-output-cfun) :void ; return void
  ( audiob-in  (:pointer (:struct audiobuffer-struct) ) )
  ( audiob-out (:pointer (:struct audiobuffer-struct) ) )
  ( node-id :unsigned-int )
)


(defmethod* simulate-propagation ( om-sound-in nodes-pos emitter-id propag-speed propag-gain propag-rx-threshold)
  :icon '(347)
  :menuins '()
  :indoc '("om-sound" "nodes-pos" "emitter-id (start from 1)" "speed" "gain" "rx-theshold")
  :outdoc '("")
  :initvals '(nil ((0 0 0)(1 1 0)) 1 343 0.9 0.1)
  :doc "simulate propagation"

  (let* ( (c-nodes-pos (allocate-list-of-list nodes-pos))
        )
        (print "running simulate-propagation")
        (unwind-protect
          (progn
            ; init c sampling rate
            ( set-sample-rate-cfun  (sample-rate om-sound-in) )
            ( set-speed-gain-rxThresh-cfun (coerce propag-speed 'float) propag-gain propag-rx-threshold )
            ; register topology from input positions
            ( register-topology-cfun c-nodes-pos )
            ; simulate propagation (generate propagation tree + per-node IR etc.)
            ( simulate-propagation-cfun emitter-id )

            ; prepare output pointers (to sound buffer)
            (let* ( 
                    (c-audiobuffer-in (allocate-audio-buffer om-sound-in))
                    (n-channels-out 1)
                    (n-samples-out (+ (n-samples om-sound-in) (ceiling (* (sample-rate om-sound-in) (get-max-propagation-time-cfun)))) )
                    (om-sound-out (make-om-sound-instance n-channels-out n-samples-out (sample-rate om-sound-in)))
                    (c-audiobuffer-out (allocate-audio-buffer om-sound-out))
                  )

                  (unwind-protect
                    (progn

                      (loop for node-id from 1 to (length nodes-pos) do 

                        ; generate sound from IR and store in in output audio buffer
                        (  convolve-input-to-output-cfun c-audiobuffer-in c-audiobuffer-out node-id ) 
                        ; fill in om sound buffer with data values from audio-buffer
                        (dotimes (c n-channels-out)
                          (dotimes (smp n-samples-out)
                            (setf (fli:dereference (fli:dereference (om-sound-buffer-ptr (buffer om-sound-out)) :index c :type :pointer)
                                                   :index smp :type :float)
                                  (fli:dereference (fli:dereference (cffi:foreign-slot-value c-audiobuffer-out '(:struct audiobuffer-struct) 'data) :index c :type :pointer) :index smp :type :float)
                                  )))

                        ; collect (clone-object om-sound-out) ; seems to keep same buffer across clones
                        collect (make-om-sound-hard-copy om-sound-out)

                      )

                    
                    )
                    ; cleanup forms
                    (progn
                      (free-audio-buffer c-audiobuffer-in (n-channels om-sound-in) )
                      (free-audio-buffer c-audiobuffer-out (n-channels om-sound-out) ) 
                      ; (om-audio::om-free-sound-buffer (buffer om-sound-out) (n-channels om-sound-out))
                    )
                  )
            )
          )
          
          ; cleanup forms
          (progn ; only use here to make sure cleaning form stays in a single block
            (free-list-of-list c-nodes-pos (length nodes-pos) )
          )
        )
  )
)

;;;======================================================================

; MISC.
; make instance of om-sound and associated sound buffer 
(defun make-om-sound-instance ( n-channels-out n-samples-out sample-rate-out )
  ; create an om-sound-buffer used for output
  (let* ( (om-buffer-out (make-om-sound-buffer-gc 
                  :nch n-channels-out
                  :ptr (make-audio-buffer n-channels-out n-samples-out)))
        )

        ; create associated om-sound, used to fill output list
        (om-init-instance (make-instance 'sound :buffer om-buffer-out
                                         :n-samples n-samples-out
                                         :n-channels n-channels-out
                                         :sample-rate sample-rate-out )
                          `((:file , nil)))
  )
)

(defun make-om-sound-hard-copy (om-sound-in)
  (let* ( (om-sound-out (make-om-sound-instance (n-channels om-sound-in) (n-samples om-sound-in) (sample-rate om-sound-in)))
          (ptr-in  (oa::om-pointer-ptr (buffer om-sound-in)))
          (ptr-out (oa::om-pointer-ptr (buffer om-sound-out)))
        )

        (loop for ch from 0 below (n-channels om-sound-in) do
        ;   (let  ( (ch-ptr-in (om-read-ptr  (om-sound-buffer-ptr (buffer om-sound-in) ) ch :pointer))
        ;           (ch-ptr-out (om-read-ptr (om-sound-buffer-ptr (buffer om-sound-out)) ch :pointer))
        ;         )

                (loop for i from 0 below (n-samples om-sound-in) do
                  (setf 
                    (fli:dereference (fli:dereference ptr-out :index ch :type :pointer) :index i :type :float)
                    (fli:dereference (fli:dereference ptr-in  :index ch :type :pointer) :index i :type :float)
                  )
        ;           (setf (om-read-ptr ch-ptr-out i :float) (om-read-ptr ch-ptr-in i :float) ))
        ;         )
        ))
        om-sound-out


  ))

;;;======================================================================

; OM-SOUND C STRUCT RELATED FUNCTIONS

; allocate audio buffer structure fields from input om-sound data
(defun allocate-audio-buffer ( om-sound ) 
(let* ( (audiob (cffi::foreign-alloc 'audiobuffer-type)) 
        (sound-buffer (om-allocate-sound-buffer om-sound) ) )
    (progn
      (setf (cffi:foreign-slot-value audiob '(:struct audiobuffer-struct) 'numChannels) (n-channels om-sound))
      (setf (cffi:foreign-slot-value audiob '(:struct audiobuffer-struct) 'numSamples) (n-samples om-sound))
      (setf (cffi:foreign-slot-value audiob '(:struct audiobuffer-struct) 'data) sound-buffer )
      audiob
    )
))

; free audio buffer
(defun free-audio-buffer (audiob numCh) 
  (om-audio::om-free-sound-buffer (cffi:foreign-slot-value audiob '(:struct audiobuffer-struct) 'data) numCh )
  ; (cffi::foreign-free (cffi:foreign-slot-value audiob '(:struct audiobuffer-struct) 'data))
  (cffi::foreign-free audiob)
)

; allocate and returns sound buffer. a sound buffer is used to fill the 'data field of an audio-buffer structure,
; it's an array of arrays (one per channel in om-sound) that contains sound data as floats. (by array understand pointer)
(defun om-allocate-sound-buffer ( om-sound ) 
  (let* (
        (nch (n-channels om-sound) )
        (size (n-samples om-sound) ) 
        )

        (fli::allocate-foreign-object 
          :type :pointer :nelems nch
          :initial-contents (loop for c from 0 to (1- nch) collect
            (fli::allocate-foreign-object :type :float :nelems size :initial-contents ( om-sound-to-list om-sound  c ) ))
        )))

; extract channel from om-sound as a list of float. returns said list
(defun om-sound-to-list ( om-sound channel )
  (with-audio-buffer (b om-sound)
    (let  ( (numdat (n-samples om-sound)) )
          
          (let ((channel-ptr (om-read-ptr (om-sound-buffer-ptr b) channel :pointer)))
          (loop for i from 0 to (1- numdat) collect
                (om-read-ptr channel-ptr i :float)))
        
    )))

; create and returns an om sound from the data in an audiobuffer-struct
(defun make-om-sound-instance-from-audio-buffer ( audiob ) 
(let* ( (n-channels-out (cffi:foreign-slot-value audiob '(:struct audiobuffer-struct) 'numChannels) )
        (n-samples-out (cffi:foreign-slot-value audiob '(:struct audiobuffer-struct) 'numSamples) )
        (out-buffer-data (cffi:foreign-slot-value audiob '(:struct audiobuffer-struct) 'data) )
        ; create an om-sound-buffer to which audiob data values will be copied
        (om-buffer-out (make-om-sound-buffer-gc 
                        :nch n-channels-out
                        :ptr (make-audio-buffer n-channels-out n-samples-out)))
      )
      
      ;fill in om sound buffer with data values from audio-buffer audiob
      (dotimes (c n-channels-out)
        (dotimes (smp n-samples-out)
          (setf (fli:dereference (fli:dereference (om-sound-buffer-ptr om-buffer-out) :index c :type :pointer)
                                 :index smp :type :float)
                (fli:dereference (fli:dereference out-buffer-data :index c :type :pointer) :index smp :type :float)
                )))

      ; create om-sound from filled buffer
      (om-init-instance (make-instance 'sound :buffer om-buffer-out
                                       :n-samples n-samples-out
                                       :n-channels n-channels-out
                                       :sample-rate 44100) ; (sample-rate om-sound-in)
                        `((:file , nil)))
))

;;;======================================================================

; LISP-LIST C STRUCT RELATED FUNCTIONS

; convert lisp list to c-ready list structure, allocating foreign values
(defun allocate-list (list-in) 
  (let* ( (list-type-in (cffi::foreign-alloc 'list-type)) )
      (setf (cffi:foreign-slot-value list-type-in '(:struct list-struct) 'size) (length list-in))
      (setf (cffi:foreign-slot-value list-type-in '(:struct list-struct) 'data) (cffi:foreign-alloc :float :initial-contents list-in))
      list-type-in
    )
)

; free pointer values allocated in allocate-list
(defun free-list (list-type-in)
    ( cffi::foreign-free (cffi:foreign-slot-value list-type-in '(:struct list-struct) 'data) )
    ( cffi::foreign-free list-type-in )
)

;;;======================================================================

; OM-LIST-OF-LIST C STRUCT RELATED FUNCTIONS

(defun number-to-double (i) (coerce i 'double-float))
(defun number-to-float (i) (coerce i 'float))

(defun coerce-list (liste type)
  (case type 
    (:double (mapcar #'number-to-double liste))
    (:float (mapcar #'number-to-float liste))
    (otherwise nil)))

; allocate audio buffer structure fields from input om-sound data
(defun allocate-list-of-list ( list-in ) 
(let* ( (lol (cffi::foreign-alloc 'list-of-list-type)) 
        (size1 (length list-in))
        ; assumes all lists in main list have same size + main list not empty + etc.
        (size2 (length (nth 1 list-in)))

        (data (fli::allocate-foreign-object 
          :type :pointer :nelems size1
          :initial-contents (loop for c from 0 to (1- size1) collect
            (fli::allocate-foreign-object :type :float :nelems size2 :initial-contents (coerce-list (nth c list-in) :float) ))
        ))
      )
      (progn
        (setf (cffi:foreign-slot-value lol '(:struct list-of-list-struct) 'size1) size1)
        (setf (cffi:foreign-slot-value lol '(:struct list-of-list-struct) 'size2) size2 )
        (setf (cffi:foreign-slot-value lol '(:struct list-of-list-struct) 'data) data )
        lol
      )
))

; free audio buffer
(defun free-list-of-list (lol size1)
  (when size1 (dotimes (c size1) (fli::free-foreign-object (fli:dereference (cffi:foreign-slot-value lol '(:struct list-of-list-struct) 'data) :type :pointer :index c))))
  (cffi::foreign-free lol)
)

