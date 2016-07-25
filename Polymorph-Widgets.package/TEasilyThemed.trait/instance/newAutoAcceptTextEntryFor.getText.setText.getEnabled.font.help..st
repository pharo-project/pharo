controls
newAutoAcceptTextEntryFor: aModel getText: getSel setText: setSel getEnabled: enabledSel font: aFont help: helpText
	"Answer a text entry for the given model."

	^self theme
		newAutoAcceptTextEntryIn: self
		for: aModel
		get: getSel
		set: setSel
		class: String
		getEnabled: enabledSel
		font: aFont
		help: helpText
