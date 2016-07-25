private
primCreateCanvas

	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^self nbCall: #( AthensCairoCanvas cairo_create (self) )