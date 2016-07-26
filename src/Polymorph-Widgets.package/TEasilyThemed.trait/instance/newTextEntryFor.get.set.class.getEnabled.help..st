controls
newTextEntryFor: aModel get: getSel set: setSel class: aClass getEnabled: enabledSel help: helpText
	"Answer a text entry for the given model."

	^self theme
		newTextEntryIn: self
		for: aModel
		get: getSel
		set: setSel
		class: aClass
		getEnabled: enabledSel 
		help: helpText