primitives
translateX: px Y: py

	<primitive: #primitiveNativeCall module: #NativeBoostPlugin>
	
	^self nbCall: #(
		void cairo_translate (AthensCairoCanvas canvas,
                                                         double px,
                                                         double py))

