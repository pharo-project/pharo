rendering
renderGlyphs: cairoGlyphs

	font lock.
	canvas 
		setPathMatrix;
		setScaledFont: font.
	canvas paint loadOnCairoCanvas: canvas.

	canvas showGlyphs: cairoGlyphs getHandle size: cairoGlyphs size.
	font unlock.

	
