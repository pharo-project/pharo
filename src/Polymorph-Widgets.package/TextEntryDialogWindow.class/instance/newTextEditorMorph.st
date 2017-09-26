creation
newTextEditorMorph
	"Answer a new text entry morph."

	^(self
		newTextEntryFor: self
		getText: #entryText
		setText: #entryText:
		getEnabled: nil
		help: nil)
		acceptOnCR: false;
		selectAll