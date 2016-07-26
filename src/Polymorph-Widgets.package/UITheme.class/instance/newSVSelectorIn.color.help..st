morph creation
newSVSelectorIn: aThemedMorph color: aColor help: helpText
	"Answer a saturation-volume selector."

	^SVColorSelectorMorph new
		color: aColor;
		selectedColor: aColor;
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		setBalloonText: helpText