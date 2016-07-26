private
primWidth: aWidth height: aHeight
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin>
	
	^self nbCall: #( AthensCairoSurface cairo_image_surface_create (
						CAIRO_FORMAT_ARGB32,
						int aWidth,
						int aHeight) )