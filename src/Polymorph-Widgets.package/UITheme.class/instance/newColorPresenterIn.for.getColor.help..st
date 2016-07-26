morph creation
newColorPresenterIn: aThemedMorph for: aModel getColor: getSel help: helpText
	"Answer a color presenter."

	^(ColorPresenterMorph
			on: aModel color: getSel)
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		setBalloonText: helpText