primitives
primAddColorStopOffset: offset R: red G: green B: blue A: alpha 
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin>
	
	^ self nbCall:#( void cairo_pattern_add_color_stop_rgba ( cairo_pattern_t  self,
					double offset,
					double red,
					double green,
					double blue,
					double alpha ))
		