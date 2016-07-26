private
primCreate: face fontMatrix: fontMatrix userToDeviceMatrix: ctm options: options
"
cairo_scaled_font_t * cairo_scaled_font_create          (cairo_font_face_t *font_face,
                                                         const cairo_matrix_t *font_matrix,
                                                         const cairo_matrix_t *ctm,
                                                         const cairo_font_options_t *options);


 Creates a cairo_scaled_font_t object from a font face and matrices that describe the size of the font and the environment in which it will be used.

font_face :
	a cairo_font_face_t

font_matrix :
	font space to user space transformation matrix for the font. In the simplest case of a N point font, this matrix is just a scale by N, but it can also be used to shear the font or stretch it unequally along the two axes. See cairo_set_font_matrix().

ctm :
	user to device transformation matrix with which the font will be used.

options :
	options to use when getting metrics for the font and rendering with it.

Returns :
	a newly created cairo_scaled_font_t. Destroy with cairo_scaled_font_destroy()
"
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	
	^ self nbCall: #( 
		CairoScaledFont cairo_scaled_font_create (
			CairoFontFace face,
			AthensCairoMatrix * fontMatrix,
			AthensCairoMatrix * ctm,
			CairoFontOptions  options ) )
 