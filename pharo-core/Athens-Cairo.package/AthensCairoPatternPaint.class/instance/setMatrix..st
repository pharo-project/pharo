accessing
setMatrix: m

	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	
	self nbCall: #( 
		void
			cairo_pattern_set_matrix ( self, AthensCairoMatrix * m)
		)
