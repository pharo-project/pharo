primitives
scaleX: fx Y: fy

	<primitive: #primitiveNativeCall module: #NativeBoostPlugin>
	
	^self nbCall: #(
		void cairo_scale (AthensCairoCanvas canvas,
                                                         double fx,
                                                         double fy))

