initialization
initializeActionButtonNamed: aString
	actionButton := PluggableButtonMorph
		newButtonFor: self
		action: #execute
		label: aString
		help: nil