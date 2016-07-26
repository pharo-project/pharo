primitives
moveToX: px Y: py

	<primitive: #primitiveNativeCall module: #NativeBoostPlugin>
	
	^self nbCall: #(
		void cairo_move_to (AthensCairoCanvas canvas,
                                                         double px,
                                                         double py))

