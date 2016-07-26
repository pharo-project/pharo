as yet unclassified
setCurrentFont: aFont
	font := aFont.
	glyphRenderer := font glyphRendererOn: canvas surface.
	glyphRenderer 
		advance: advance;
		setColor: color.