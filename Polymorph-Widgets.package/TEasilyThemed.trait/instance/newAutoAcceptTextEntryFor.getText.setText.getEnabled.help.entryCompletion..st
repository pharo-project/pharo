controls
newAutoAcceptTextEntryFor: aModel getText: getSel setText: setSel getEnabled: enabledSel help: helpText entryCompletion: anEntryCompletion
	"Answer a text entry for the given model."

	^self theme
		newAutoAcceptTextEntryIn: self
		for: aModel
		get: getSel
		set: setSel
		class: String
		getEnabled: enabledSel
		help: helpText
		entryCompletion: anEntryCompletion