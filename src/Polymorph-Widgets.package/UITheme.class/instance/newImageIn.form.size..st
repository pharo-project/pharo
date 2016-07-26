morph creation
newImageIn: aThemedMorph form: aForm size: aPoint
	"Answer a new image morph."

	^AlphaImageMorph new
		borderStyle: (BorderStyle inset width: 0);
		image: aForm size: aPoint