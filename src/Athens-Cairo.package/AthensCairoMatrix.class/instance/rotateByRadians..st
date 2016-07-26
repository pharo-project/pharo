transformations
rotateByRadians: angle

	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	
	^self nbCall: #(
		void cairo_matrix_rotate (AthensCairoMatrix *  self,
                                                         double angle))

