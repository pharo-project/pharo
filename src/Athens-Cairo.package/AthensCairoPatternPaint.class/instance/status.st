accessing
status
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin>
	
	^ self nbCall: #(int cairo_pattern_status (cairo_pattern_t  self))
		