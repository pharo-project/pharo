primitives
primTransformX: x Y: y

	<primitive: #primitiveNativeCall module: #NativeBoostPlugin>
	
	^self nbCall: #(
		void cairo_user_to_device (AthensCairoCanvas canvas,
                                                         NBFloatPtr x,
                                                         NBFloatPtr y) )
