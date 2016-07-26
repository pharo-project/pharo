morph creation
newAutoAcceptTextEditorIn: aThemedMorph for: aModel getText: getSel setText: setSel getEnabled: enabledSel
	"Answer a text editor for the given model."

	^PluggableTextMorph new
		autoAccept: true;
		on: aModel
		text: getSel
		accept: setSel
		readSelection: nil
		menu: nil;
		getEnabledSelector: enabledSel;
		font: self textFont;
		cornerStyle: aThemedMorph preferredCornerStyle;
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		borderStyle: (BorderStyle inset width: 1);
		selectionColor: self selectionColor