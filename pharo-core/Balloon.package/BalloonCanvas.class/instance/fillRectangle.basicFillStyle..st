balloon drawing
fillRectangle: aRectangle basicFillStyle: aFillStyle
	"Fill the given rectangle with the given, non-composite, fill style."
	
	^self drawRectangle: aRectangle
			color: aFillStyle
			borderWidth: 0
			borderColor: nil
