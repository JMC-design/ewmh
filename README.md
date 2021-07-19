# ewmh
CL implementation of Extended Window Manager Hints

Naming follows the spec with the exception that underscores are replaced with hypens and _net and _net_wm are removed. e.g _net_wm_visible_name becomes ewmh:visible-name.  This library is not meant to be :USE'd.
Properties may be changed either by prefixing the property by SET-, e.g. (ewmh:set-name window name) or by setf'ing the property,e.g. (setf (ewmh:name window) name).

Properties that are controlled by the windowmanager can be requested to change by postfixing the property with -request, only for those in the spec obviously, e.g. (ewmh:desktop-request root 2) to request a change to desktop 2 on the specified root.
