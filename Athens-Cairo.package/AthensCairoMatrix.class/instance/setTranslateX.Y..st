transformations
setTranslateX: x Y: y

	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	
	^self nbCall: #( void cairo_matrix_init_translate (AthensCairoMatrix * self, double  x, double  y))

