morph creation
newCancelButtonIn: aThemedMorph for: aModel
	"Answer a new cancel button."

	^self
		newButtonIn: aThemedMorph
		for: aModel
		getState: nil
		action: #cancel
		arguments: nil
		getEnabled: nil
		label: 'Cancel' translated
		help: 'Cancel changes and close the window' translated