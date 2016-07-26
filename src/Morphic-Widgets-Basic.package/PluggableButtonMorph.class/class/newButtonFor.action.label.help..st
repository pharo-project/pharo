instance creation
newButtonFor: aModel action: actionSel label: stringOrText help: helpText
	"Answer a new button."

	^self
		newButtonFor: aModel
		getState: nil
		action: actionSel
		arguments: nil
		getEnabled: nil 
		label: stringOrText
		help: helpText