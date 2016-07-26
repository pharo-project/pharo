controls
newAutoAcceptTextEntryFor: aModel get: getSel set: setSel class: aClass getEnabled: enabledSel help: helpText entryCompletion: anEntryCompletion
	"Answer a text entry for the given model."

	^self theme
		newAutoAcceptTextEntryIn: self
		for: aModel
		get: getSel
		set: setSel
		class: aClass
		getEnabled: enabledSel
		help: helpText
		entryCompletion: anEntryCompletion