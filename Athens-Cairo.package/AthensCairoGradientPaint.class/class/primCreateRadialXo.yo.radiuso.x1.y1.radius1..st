primitives
primCreateRadialXo: xo yo: yo radiuso: radiuso x1: x1 y1: y1 radius1: radius1
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin>
	
	^self nbCall: #(AthensCairoGradientPaint cairo_pattern_create_radial (
		double xo,
		double yo,
		double radiuso,
		double x1,
		double y1,
		double radius1 ) )
