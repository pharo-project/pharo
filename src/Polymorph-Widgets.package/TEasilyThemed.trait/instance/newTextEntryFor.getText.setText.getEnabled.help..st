controls
newTextEntryFor: aModel getText: getSel setText: setSel getEnabled: enabledSel help: helpText
	"Answer a text entry for the given model."

	^self theme
		newTextEntryIn: self
		for: aModel
		get: getSel
		set: setSel
		class: String
		getEnabled: enabledSel 
		help: helpText