controls
newButtonFor: aModel getState: stateSel action: actionSel arguments: args getEnabled: enabledSel getLabel: labelSel help: helpText
	"Answer a new button."

	^self theme
		newButtonIn: self for: aModel
		getState: stateSel
		action: actionSel
		arguments: args
		getEnabled: enabledSel 
		getLabel: labelSel
		help: helpText