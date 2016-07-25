controls
newColorChooserFor: aModel getColor: getSel setColor: setSel getEnabled: enabledSel help: helpText
	"Answer a color chooser with the given selectors."

	^self theme
		newColorChooserIn: self
		for: aModel
		getColor: getSel
		setColor: setSel
		getEnabled: enabledSel
		help: helpText