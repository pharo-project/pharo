private
setSourceR: red g: green b: blue a: alpha

	<primitive: #primitiveNativeCall module: #NativeBoostPlugin>
	
	^ self nbCall: #( 
		void cairo_set_source_rgba ( self ,
			double red,
			double green,
			double blue,
			double alpha) )