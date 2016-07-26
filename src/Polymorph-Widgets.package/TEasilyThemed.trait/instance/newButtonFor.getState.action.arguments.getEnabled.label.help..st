controls
newButtonFor: aModel getState: stateSel action: actionSel arguments: args getEnabled: enabledSel label: stringOrText help: helpText
	"Answer a new button."

	^self theme
		newButtonIn: self for: aModel
		getState: stateSel
		action: actionSel
		arguments: args
		getEnabled: enabledSel 
		label: stringOrText
		help: helpText