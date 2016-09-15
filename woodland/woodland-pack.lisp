(in-package :om)

(let ((soundpack (omNG-make-package "Woodland"
                   :container-pack *om-package-tree*
                   :doc "Simulating sound propagation in a forest-like environment"
                   :classes '()
                   :functions '( simulate-propagation )
                   )))
  )