transformations
setScaleX: x Y: y

	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	
	^self nbCall: #( void cairo_matrix_init_scale (AthensCairoMatrix * self, double  x, double  y))

