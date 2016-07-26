private
rectangleX: x y: y width: aWidth height: aHeight
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin>
	
	^ self nbCall: #(void cairo_rectangle (self,
		 double x, double y,
		 double aWidth, double aHeight) )
