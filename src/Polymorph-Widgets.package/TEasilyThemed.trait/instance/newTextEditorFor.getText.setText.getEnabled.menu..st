controls
newTextEditorFor: aModel getText: getSel setText: setSel getEnabled: enabledSel menu: menuSelector
	"Answer a text editor for the given model."

	^self theme
		newTextEditorIn: self
		for: aModel
		getText: getSel
		setText: setSel
		getEnabled: enabledSel
		menu: menuSelector