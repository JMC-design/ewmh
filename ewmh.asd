(in-package :asdf-user)
(defsystem "ewmh"
  :author "Johannes Martinez Calzada"
  :description "Implementation of Extended Window Manager Hints v1.3 https://specifications.freedesktop.org/wm-spec/wm-spec-1.3.html"
  :licence "llgpl"
  :depends-on ("clx" "split-sequence" "babel")
  :components ((:file "ewmh")))
