surface plugin callbacks
get_data: surface
	"WARNING: while the function receives a pointer to surface I receive a long number (and address)
	 There is no point on doing all the conversions so I'm just assuming the surface is just a number 
	 (otherwise correct parameter should be 'cairo_surface_t *' (an AthensCairoSurface). 
	 And same happens with return: it should be 'unsigned char *' but since I use it to return the
	 address, an ulong avoids conversions (then being faster)"
	^ self ffiCall: #(ulong cairo_image_surface_get_data (ulong surface))