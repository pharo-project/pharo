converting
asExternalTypeOn: generator 
	"use handle ivar to hold my instance (cairo_surface_t)"
	^ FFIExternalObjectType objectClass: self