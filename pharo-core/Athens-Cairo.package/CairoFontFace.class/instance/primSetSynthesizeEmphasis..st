private
primSetSynthesizeEmphasis: flags
"
void
cairo_ft_font_face_set_synthesize (cairo_font_face_t *font_face,
                                   int synth_flags);
"
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	
	^ self nbCall: #( void cairo_ft_font_face_set_synthesize(self, int flags )) 