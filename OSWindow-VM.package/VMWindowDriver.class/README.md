This driver using a legacy VM functionality which will be removed in future versions,
because it is limited only to a single OS Window, and furthermore, image don't have a direct control over window creation/destruction neither controlling its various attributes and event delivery and handling mechanisms.

This driver is used by image, when it detects there's no other more suitable driver(s) available on currently running platform.