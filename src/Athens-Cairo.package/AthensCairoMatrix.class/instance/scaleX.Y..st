transformations
scaleX: fx Y: fy

	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	
	^self nbCall: #(
		void cairo_matrix_scale (AthensCairoMatrix * self,
                                                         double fx,
                                                         double fy))

