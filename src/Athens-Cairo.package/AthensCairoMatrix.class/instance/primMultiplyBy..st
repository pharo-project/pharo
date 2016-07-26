transformations
primMultiplyBy: m
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>

	^self nbCall: #(void   cairo_matrix_multiply
		(AthensCairoMatrix * self,
		AthensCairoMatrix * m ,
		AthensCairoMatrix * self ) )

