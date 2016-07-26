controls
newButtonFor: aModel getState: stateSel action: actionSel arguments: args getEnabled: enabledSel labelForm: aForm help: helpText
	"Answer a new button."

	^self theme
		newButtonIn: self for: aModel
		getState: stateSel
		action: actionSel
		arguments: args
		getEnabled: enabledSel 
		label: (AlphaImageMorph new image: aForm)
		help: helpText