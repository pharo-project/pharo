private
getOperator
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	
	^ self nbCall: #(
		cairo_operator_t cairo_get_operator (AthensCairoCanvas canvas) )
