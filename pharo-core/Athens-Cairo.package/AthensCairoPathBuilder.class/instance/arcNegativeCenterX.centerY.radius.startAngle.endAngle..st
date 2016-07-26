private
arcNegativeCenterX: xc centerY: yc radius: radius startAngle: angle1 endAngle: angle2 
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	
	self nbCall: #(void cairo_arc_negative (AthensCairoCanvas context,
				double xc,
				double yc,
				double radius,
				double angle1,
				double angle2) )
				
			