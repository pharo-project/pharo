drawing
drawWithAlpha: alpha

	<primitive: #primitiveNativeCall module: #NativeBoostPlugin>
	
	^ self nbCall: #( 
		void cairo_paint_with_alpha ( self , double alpha)
	)