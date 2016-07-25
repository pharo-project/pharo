morph creation
newAutoAcceptTextEntryIn: aThemedMorph for: aModel  get: getSel set: setSel class: aClass getEnabled: enabledSel help: helpText entryCompletion: anEntryCompletion
	"Answer a text entry for the given model.
	Object conversion will be automatic based on the class of
	object returned after performing the get selector, aClass
	is provided in case of initial nil returns."

	^self
		newAutoAcceptTextEntryIn: aThemedMorph
		for: aModel
		get: getSel
		set: setSel
		class: aClass
		getEnabled: enabledSel
		font: self textFont
		help: helpText
		entryCompletion: anEntryCompletion