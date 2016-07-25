actions
newColorComponentFieldMorph: aspect
	"Answer a text entry for the specified aspect of the color."

	^(self
		newTextEntryFor: self
		get: aspect
		set: (aspect, ':') asSymbol
		class: Integer
		getEnabled: nil
		help: nil)
		acceptOnFocusChange: true;
		minWidth: 40