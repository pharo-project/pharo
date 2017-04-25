surface plugin callbacks
get_width: surface
	^ self ffiCall: #(int cairo_image_surface_get_width (void *surface))