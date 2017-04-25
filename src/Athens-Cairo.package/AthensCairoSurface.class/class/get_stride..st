surface plugin callbacks
get_stride: surface
	^ self ffiCall: #(int cairo_image_surface_get_stride (void *surface))