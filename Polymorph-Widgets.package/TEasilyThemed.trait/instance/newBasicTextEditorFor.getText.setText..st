controls
newBasicTextEditorFor: aModel getText: getSel setText: setSel
	"Answer a text editor for the given model."

	^self
		newBasicTextEditorFor: aModel
		getText: getSel
		setText: setSel
		getEnabled: nil