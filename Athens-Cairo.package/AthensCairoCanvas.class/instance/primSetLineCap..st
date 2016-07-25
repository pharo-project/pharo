private
primSetLineCap: capStyle
	
	^ self ffiCall: #(void cairo_set_line_cap ( self, cairo_line_cap_t capStyle ) )