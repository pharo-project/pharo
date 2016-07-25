controls
newColorChooserFor: aModel getColor: getSel setColor: setSel help: helpText
	"Answer a color chooser with the given selectors."

	^self theme
		newColorChooserIn: self
		for: aModel
		getColor: getSel
		setColor: setSel
		getEnabled: nil
		help: helpText