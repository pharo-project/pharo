private
flush

	<primitive: #primitiveNativeCall module: #NativeBoostPlugin>
	
	^self nbCall: #( void cairo_surface_flush (self) )