instance creation
primFtFace: aFace loadFlags: flags

"
cairo_font_face_t * cairo_ft_font_face_create_for_ft_face
                                                        (FT_Face face,
                                                         int load_flags);
"
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	
	^ self nbCall: #( CairoFontFace cairo_ft_font_face_create_for_ft_face(void * aFace , int flags )) 