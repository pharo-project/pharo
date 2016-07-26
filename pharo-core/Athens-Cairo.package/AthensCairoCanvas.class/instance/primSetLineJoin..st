private
primSetLineJoin: joinStyle

	self ffiCall: #(void cairo_set_line_join ( self, cairo_line_join_t joinStyle) )