private
primSetLineJoinOn: aCanvas
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin>
	
	self nbCall: #(void cairo_set_line_join (
			AthensCairoCanvas aCanvas,
			cairo_line_join_t joinStyle) )