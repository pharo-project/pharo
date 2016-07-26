private
primSetLineWidthOn: aCanvas
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin>
	
	self nbCall: #(void cairo_set_line_width (
				AthensCairoCanvas aCanvas,
				double width) )
