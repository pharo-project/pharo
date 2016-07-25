morph creation
newAlphaSelectorIn: aThemedMorph for: aModel getAlpha: getSel setAlpha: setSel help: helpText
	"Answer an alpha selector ."

	^(AColorSelectorMorph
			on: aModel
			getValue: getSel
			setValue: setSel)
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		setBalloonText: helpText