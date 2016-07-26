primitives
loadIdentity

	<primitive: #primitiveNativeCall module: #NativeBoostPlugin>
	
	^self nbCall: #(
		void cairo_identity_matrix (AthensCairoCanvas canvas) )

