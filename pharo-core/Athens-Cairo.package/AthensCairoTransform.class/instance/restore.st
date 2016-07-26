primitives
restore
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin>
	
	^ self nbCall: #(void cairo_restore(AthensCairoCanvas canvas))
