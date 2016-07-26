transformations
invert

	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	
	^self nbCall: #( cairo_status_t cairo_matrix_invert (AthensCairoMatrix *  self))

