surface plugin callbacks
get_data: surface
	^ self ffiCall: #(void *cairo_image_surface_get_data (void *surface))