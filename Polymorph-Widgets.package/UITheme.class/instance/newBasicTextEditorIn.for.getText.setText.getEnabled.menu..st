morph creation
newBasicTextEditorIn: aThemedMorph for: aModel getText: getSel setText: setSel getEnabled: enabledSel menu: aMenuSelector
	"Answer a text editor (not Smalltalk code) for the given model."

	^PluggableTextEditorMorph new
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
		color: Color white;
		selectionColor: self selectionColor;
		removeProperty: #theme