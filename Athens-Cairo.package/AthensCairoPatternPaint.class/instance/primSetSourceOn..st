primitives
primSetSourceOn: aCanvas
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin>
	
	^ self nbCall: #(void cairo_set_source ( cairo_t  aCanvas,  self))