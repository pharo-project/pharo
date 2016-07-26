border-styles
drawTextAdornmentFor: aPluggableTextMorph color: aColor on: aCanvas
	"Indicate edit status for the given morph."
	
	aCanvas frameRectangle: aPluggableTextMorph innerBounds width: 2 color: aColor