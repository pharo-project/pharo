controls
newColorPresenterFor: aModel getColor: getSel help: helpText
	"Answer a color presenter with the given selectors."

	^self theme
		newColorPresenterIn: self
		for: aModel
		getColor: getSel
		help: helpText