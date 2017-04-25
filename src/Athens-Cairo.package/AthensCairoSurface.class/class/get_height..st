surface plugin callbacks
get_height: surface
	^ self ffiCall: #(int cairo_image_surface_get_height (void *surface))