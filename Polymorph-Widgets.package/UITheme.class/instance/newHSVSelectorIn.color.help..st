morph creation
newHSVSelectorIn: aThemedMorph color: aColor help: helpText
	"Answer a hue-saturation-volume selector."

	^HSVColorSelectorMorph new
		selectedColor: aColor;
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		setBalloonText: helpText