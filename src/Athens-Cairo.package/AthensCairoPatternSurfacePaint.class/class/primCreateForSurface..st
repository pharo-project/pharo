as yet unclassified
primCreateForSurface: anAthensCairoSurface
	
"	pattern = cairo_pattern_create_for_surface (image);"
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin>
	
	^self nbCall: #(AthensCairoPatternSurfacePaint cairo_pattern_create_for_surface(
		cairo_surface_t  anAthensCairoSurface ) )
