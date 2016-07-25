operations
newHColorMorph
	"Answer a new hue color morph."

	^HColorSelectorMorph new
		model: self;
		setValueSelector: #hue:;
		hResizing: #rigid;
		vResizing: #spaceFill;
		extent: 36@36