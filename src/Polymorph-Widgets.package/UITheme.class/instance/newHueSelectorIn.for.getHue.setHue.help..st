morph creation
newHueSelectorIn: aThemedMorph for: aModel getHue: getSel setHue: setSel help: helpText
	"Answer a hue selector ."

	^(HColorSelectorMorph
			on: aModel
			getValue: getSel
			setValue: setSel)
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		setBalloonText: helpText