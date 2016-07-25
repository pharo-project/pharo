primitives
primCreateLinearX0: x0 y0: y0 x1: x1 y1: y1
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin>
	
	^self nbCall: #(AthensCairoGradientPaint cairo_pattern_create_linear (
		double x0,
		double y0,
		double x1,
		double y1 ) )
