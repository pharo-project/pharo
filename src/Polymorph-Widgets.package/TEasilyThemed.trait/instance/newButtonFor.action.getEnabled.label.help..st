controls
newButtonFor: aModel action: actionSel getEnabled: enabledSel label: stringOrText help: helpText
	"Answer a new button."

	^self
		newButtonFor: aModel
		getState: nil
		action: actionSel
		arguments: nil
		getEnabled: enabledSel 
		label: stringOrText
		help: helpText