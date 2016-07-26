surface plugin callbacks
get_stride: surface
	"WARNING: while the function receives a pointer to surface I receive a long number (and address)
	 There is no point on doing all the conversions so I'm just assuming the surface is just a number 
	 (otherwise correct parameter should be 'cairo_surface_t *' (an AthensCairoSurface)."
	^ self ffiCall: #(int cairo_image_surface_get_stride (ulong surface))