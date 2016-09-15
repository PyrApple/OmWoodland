(in-package :om)

; define the dynamic library
(cffi::define-foreign-library libomwoodland
   (t (:default "/Applications/OPENMUSIC/packages/woodland/libOmWoodland")))
   ; ( 
   ; 	(:macosx (om-relative-path nil "libOmCwrap") ) )
   ; 	(t (:default "libomspat"))
   ; )


(compile&load (om-relative-path '("src") "woodland-src"))

(compile&load (om-relative-path nil "woodland-pack"))