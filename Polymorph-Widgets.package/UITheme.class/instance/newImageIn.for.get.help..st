morph creation
newImageIn: aThemedMorph for: aModel get: getSel help: helpText
	"Answer a new image morph."

	^AlphaImageMorph new
		model: aModel;
		borderStyle: (BorderStyle inset width: 0);
		getImageSelector: getSel;
		setBalloonText: helpText;
		updateImage