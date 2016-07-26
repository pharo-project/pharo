private
primDestroyPattern: aHandle
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	
	self nbCall: #(void cairo_pattern_destroy (ulong aHandle) )
 