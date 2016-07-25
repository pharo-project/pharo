private
primResetDashes: anOffset
"	void                cairo_set_dash                      (cairo_t *cr,
                                                         double *dashes,
                                                         int num_dashes,
                                                         double offset);"
<primitive: #primitiveNativeCall module: #NativeBoostPlugin>
	
	self nbCall: #(void cairo_set_dash (
				self,
				0,
				0,
				double anOffset) )
