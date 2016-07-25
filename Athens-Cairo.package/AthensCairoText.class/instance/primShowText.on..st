private
primShowText: aString on: aCanvas
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin>
	self nbCall: #(void cairo_show_text (
			cairo_t aCanvas,
			char * aString) )
