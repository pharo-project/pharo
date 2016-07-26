finalizing
finalizeResourceData: handle

"
void                cairo_font_face_destroy             (cairo_font_face_t *font_face);
"
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	
	^ self nbCall: #( void cairo_font_face_destroy (size_t handle)) 