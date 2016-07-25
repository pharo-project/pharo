drawing
fillColor: aColor
	"Fill the receiver with the given color.
	Note: This method should be named differently since it is intended to fill the background and thus fills even if the color is transparent"
	^self fillRectangle: self clipRect color: (aColor alpha: 1.0).