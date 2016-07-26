private
primSetDashesLengths: dashesLengths count: dashesCount offset: anOffset
	
	self ffiCall: #(void cairo_set_dash (self,
			double * dashesLengths,
			int dashesCount,
			double anOffset ) )