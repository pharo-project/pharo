private
destroyContextHandle: aHandle

	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	
	^self nbCall: #( 	void cairo_destroy (size_t aHandle) )