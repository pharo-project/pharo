private
destroySurfaceHandle: handle
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	
	^self nbCall: #( void cairo_surface_destroy ( ulong handle ) )

