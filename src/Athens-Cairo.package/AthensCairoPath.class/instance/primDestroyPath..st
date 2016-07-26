instance creation
primDestroyPath: aHandle

	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	
	^ self nbCall: #(void cairo_path_destroy ( ulong aHandle ))
