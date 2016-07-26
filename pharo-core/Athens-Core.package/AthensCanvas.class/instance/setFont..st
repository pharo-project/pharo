drawing text
setFont: aFont
	"Set the current font of receiver. Font object should answer a glyph renderer instance,
	compatible with glyph renderer protocol" 
	^ fontRenderer := aFont glyphRendererOn: surface.