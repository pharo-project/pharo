private
primSetFontSize: aFontSize on: aCanvas
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin>
	self nbCall: #(void cairo_set_font_size (
			cairo_t aCanvas,
			double aFontSize) )
