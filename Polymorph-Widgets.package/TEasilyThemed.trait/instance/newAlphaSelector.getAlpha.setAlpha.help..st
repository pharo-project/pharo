controls
newAlphaSelector: aModel getAlpha: getSel setAlpha: setSel help: helpText
	"Answer an alpha channel selector with the given selectors."

	^self theme
		newAlphaSelectorIn: self
		for: aModel
		getAlpha: getSel
		setAlpha: setSel
		help: helpText