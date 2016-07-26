accessing
stride
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin>
	
	^self nbCall: #(
		int cairo_image_surface_get_stride ( self ) )

