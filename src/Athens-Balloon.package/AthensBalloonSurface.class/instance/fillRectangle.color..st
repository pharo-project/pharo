drawing
fillRectangle: rect color: aColor

	engine
		prepareForRendering;
		registerColorFill: aColor	;
		fillRectangle: rect
