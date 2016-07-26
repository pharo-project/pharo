transformations
setRotationInRadians: radians

	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	
	^self nbCall: #( void cairo_matrix_init_rotate (AthensCairoMatrix * self, double  radians))

