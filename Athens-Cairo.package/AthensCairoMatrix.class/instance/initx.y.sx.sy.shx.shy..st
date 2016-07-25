private
initx: x y: y sx: sx sy: sy shx: shx shy: shy

	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	
	^self nbCall: #(
		void cairo_matrix_init (AthensCairoMatrix * self,
			double sx,
			double shy,
			double shx,
			double sy,
			double x,
			double y))

