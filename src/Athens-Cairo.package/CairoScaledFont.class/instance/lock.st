locking
lock

"
FT_Face             cairo_ft_scaled_font_lock_face      (cairo_scaled_font_t *scaled_font);

"
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	
	^ self nbCall: #( 
		void cairo_ft_scaled_font_lock_face (self)) 