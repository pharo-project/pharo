private
statusToString: aCairoStatusT
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin>
	^ self nbCall: #(String   cairo_status_to_string  (cairo_status_t aCairoStatusT))