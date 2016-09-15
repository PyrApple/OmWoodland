(in-package :om)

(let ((soundpack (omNG-make-package "Woodland"
                   :container-pack *om-package-tree*
                   :doc "Simulating sound propagation in a forest-like environment"
                   :classes '()
                   :functions '( simulate-propagation unpack-3dc-to-list-of-3dc point-pairs-mod)
                   )))
  )