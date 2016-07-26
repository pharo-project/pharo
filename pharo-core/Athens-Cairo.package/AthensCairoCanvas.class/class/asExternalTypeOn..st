converting
asExternalTypeOn: generator
	"use handle ivar to hold my instance (cairo_t)"
	^ FFIExternalObjectType objectClass: self