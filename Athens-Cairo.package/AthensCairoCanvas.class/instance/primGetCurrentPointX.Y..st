private
primGetCurrentPointX: x Y: y

	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	
	^self nbCall: #( void cairo_get_current_point (self, double * x, double * y))