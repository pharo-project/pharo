private
setOperator: aCairoOperatorT
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	
	^ self nbCall: #(void cairo_set_operator (AthensCairoCanvas canvas, cairo_operator_t aCairoOperatorT) )
