private
primSetLineWidth: width
	
	self ffiCall: #(void cairo_set_line_width (self, double width) )