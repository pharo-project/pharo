controls
newTextEntryFor: aModel getText: getSel setText: setSel help: helpText
	"Answer a text entry for the given model."

	^self theme
		newTextEntryIn: self
		for: aModel
		get: getSel
		set: setSel
		class: String
		getEnabled: nil 
		help: helpText