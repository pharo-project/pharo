morph creation
newColorChooserIn: aThemedMorph for: aModel getColor: getSel setColor: setSel getEnabled: enabledSel help: helpText
	"Answer a color chooser ."

	^(ColorChooserMorph
			on: aModel color: getSel changeColor: setSel)
		getEnabledSelector: enabledSel;
		hResizing: #spaceFill;
		vResizing: #shrinkWrap;
		setBalloonText: helpText