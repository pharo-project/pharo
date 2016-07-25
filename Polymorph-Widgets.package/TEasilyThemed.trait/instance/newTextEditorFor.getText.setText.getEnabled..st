controls
newTextEditorFor: aModel getText: getSel setText: setSel getEnabled: enabledSel 
	"Answer a text editor for the given model."

	^self 
		newTextEditorFor: aModel
		getText: getSel
		setText: setSel
		getEnabled: enabledSel
		menu: nil