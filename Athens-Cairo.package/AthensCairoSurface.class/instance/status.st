accessing
status

	<primitive: #primitiveNativeCall module: #NativeBoostPlugin>
	
	^ self nbCall: #(int cairo_surface_status (self) )
	