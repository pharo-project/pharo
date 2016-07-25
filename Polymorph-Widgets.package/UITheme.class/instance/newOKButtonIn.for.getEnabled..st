morph creation
newOKButtonIn: aThemedMorph for: aModel getEnabled: enabledSel
	"Answer a new OK button."

	^self
		newButtonIn: aThemedMorph
		for: aModel
		getState: nil
		action: #ok
		arguments: nil
		getEnabled: enabledSel
		label: 'OK' translated
		help: 'Apply changes and close the window' translated