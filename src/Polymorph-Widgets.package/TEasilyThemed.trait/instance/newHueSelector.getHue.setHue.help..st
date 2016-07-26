controls
newHueSelector: aModel getHue: getSel setHue: setSel help: helpText
	"Answer a hue selector with the given selectors."

	^self theme
		newHueSelectorIn: self
		for: aModel
		getHue: getSel
		setHue: setSel
		help: helpText