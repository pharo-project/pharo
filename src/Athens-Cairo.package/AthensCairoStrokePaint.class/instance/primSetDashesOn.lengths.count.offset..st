private
primSetDashesOn: aCanvas lengths: dashesLengths count: dashesCount offset: anOffset
"	void                cairo_set_dash                      (cairo_t *cr,
                                                         double *dashes,
                                                         int num_dashes,
                                                         double offset);"
<primitive: #primitiveNativeCall module: #NativeBoostPlugin>
	
	self nbCall: #(void cairo_set_dash (
				AthensCairoCanvas aCanvas,
				double* dashesLengths,
				int dashesCount,
				double anOffset) )
