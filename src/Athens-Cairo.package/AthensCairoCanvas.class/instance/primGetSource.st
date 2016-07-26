private
primGetSource
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	
	self nbCall: #( cairo_pattern_t cairo_get_source ( self ) )