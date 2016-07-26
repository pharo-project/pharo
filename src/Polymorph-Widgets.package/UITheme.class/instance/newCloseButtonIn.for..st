morph creation
newCloseButtonIn: aThemedMorph for: aModel
	"Answer a new close button."

	^self
		newButtonIn: aThemedMorph
		for: aModel
		getState: nil
		action: #close
		arguments: nil
		getEnabled: nil
		label: 'Close' translated
		help: 'Close the window' translated