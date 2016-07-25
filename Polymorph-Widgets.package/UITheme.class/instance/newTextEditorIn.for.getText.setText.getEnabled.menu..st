morph creation
newTextEditorIn: aThemedMorph for: aModel getText: getSel setText: setSel getEnabled: enabledSel menu: aMenuSelector
	"Answer a text (Smalltalk code) editor for the given model."

	^PluggableTextMorph new
		on: aModel
		text: getSel
		accept: setSel
		readSelection: nil
		menu: aMenuSelector;
		theme: self;
		getEnabledSelector: enabledSel;
		font: self textFont;
		cornerStyle: aThemedMorph preferredCornerStyle;
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		borderStyle: (BorderStyle inset width: 1);
		"color: Color darkGray;
		selectionColor: self selectionColor;"
		removeProperty: #theme