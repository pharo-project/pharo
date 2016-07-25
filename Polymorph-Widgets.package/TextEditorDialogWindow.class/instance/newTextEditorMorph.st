creation
newTextEditorMorph
	"Answer a new text entry morph."
	^(self
		newTextEditorFor: self
		getText: #entryText
		setText: #entryText:
		getEnabled: nil) 
		selectAll