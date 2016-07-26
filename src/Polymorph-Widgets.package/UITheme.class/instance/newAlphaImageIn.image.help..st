morph creation
newAlphaImageIn: aThemedMorph image: aForm help: helpText
	"Answer an alpha image morph."
	
	|answer|
	answer := AlphaImageMorph new
		hResizing: #rigid;
		vResizing: #rigid;
		setBalloonText: helpText.
	aForm ifNotNil: [answer image: aForm].
	^answer