private
destroy

	<primitive: #primitiveNativeCall module: #NativeBoostPlugin>
	
	^self nbCall: #( 	void cairo_destroy (self) )