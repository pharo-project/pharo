private
primImage: aFormat width: aWidth height: aHeight
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin>
	
	^self nbCall: #(AthensCairoSurface cairo_image_surface_create (int aFormat,
                                                         int aWidth,
                                                         int aHeight) )

