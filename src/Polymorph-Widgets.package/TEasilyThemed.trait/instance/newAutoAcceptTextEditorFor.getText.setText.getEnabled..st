controls
newAutoAcceptTextEditorFor: aModel getText: getSel setText: setSel getEnabled: enabledSel
	"Answer a text editor for the given model."

	^self theme
		newAutoAcceptTextEditorIn: self
		for: aModel
		getText: getSel
		setText: setSel
		getEnabled: enabledSel