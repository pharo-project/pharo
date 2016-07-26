finalizing
finalizeResourceData: handle

"
void                cairo_scaled_font_destroy           (cairo_scaled_font_t *scaled_font);
"
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	
	^ self nbCall: #( void  cairo_scaled_font_destroy (size_t handle) )