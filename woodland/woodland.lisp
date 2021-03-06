(in-package :om)

; define the dynamic library
(cffi::define-foreign-library libomwoodland
   (t (:default "/Applications/OPENMUSIC/packages/woodland/libOmWoodland")))

(compile&load (om-relative-path '("src") "woodland-src"))
(compile&load (om-relative-path '("src") "woodland-utils"))

(compile&load (om-relative-path nil "woodland-pack"))