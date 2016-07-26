controls
newTextEntryFor: aModel getText: getSel setText: setSel help: helpText
	"Answer a text entry for the given model."

	^self
		newTextEntryFor: aModel
		get: getSel
		set: setSel
		class: String
		getEnabled: nil 
		help: helpText