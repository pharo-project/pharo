private
primSetLineCapOn: aCanvas
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin>
	
	^ self nbCall: #(void cairo_set_line_cap (
			AthensCairoCanvas aCanvas,
			cairo_line_cap_t capStyle) )