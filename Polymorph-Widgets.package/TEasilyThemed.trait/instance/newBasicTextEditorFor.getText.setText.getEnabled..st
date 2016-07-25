controls
newBasicTextEditorFor: aModel getText: getSel setText: setSel getEnabled: enabledSel
	"Answer a text editor for the given model."

	^self
		newBasicTextEditorFor: aModel
		getText: getSel
		setText: setSel
		getEnabled: enabledSel
		menu: nil