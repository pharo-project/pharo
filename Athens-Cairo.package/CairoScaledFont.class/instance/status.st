accessing
status
"cairo_status_t      cairo_scaled_font_status            (cairo_scaled_font_t *scaled_font);"
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	
	^ self nbCall: #(int cairo_scaled_font_status (self))
		