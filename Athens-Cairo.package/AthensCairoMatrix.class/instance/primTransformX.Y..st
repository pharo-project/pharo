private
primTransformX: x Y: y

	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	
	^self nbCall: #( void cairo_matrix_transform_point (AthensCairoMatrix *  self, double * x, double * y))

