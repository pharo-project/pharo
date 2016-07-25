drawing
strokeRectangle: rect color: aColor width: aWidth
	 
	engine
		prepareForRendering;
		registerColorFill: aColor	;
		strokeRectangle: rect width: aWidth