private
primCreateFromFile: aFileName

	<primitive: #primitiveNativeCall module: #NativeBoostPlugin> 
	
	^self nbCall: #(AthensCairoSurface cairo_image_surface_create_from_png (char* aFileName)) 
