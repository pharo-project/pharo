primitives
rotateByRadians: angle

	<primitive: #primitiveNativeCall module: #NativeBoostPlugin>
	
	^self nbCall: #(
		void cairo_rotate (AthensCairoCanvas canvas,
                                                         double angle))

