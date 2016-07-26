primitives
save
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin>
	
	^ self nbCall: #(void cairo_save (AthensCairoCanvas canvas))
