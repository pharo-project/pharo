morph creation
newTextEntryIn: aThemedMorph for: aModel get: getSel set: setSel class: aClass getEnabled: enabledSel help: helpText entryCompletion: anEntryCompletion
	"Answer a text entry for the given model."

	|pt|
	pt := PluggableTextFieldMorph new
		convertTo: aClass;
		alwaysAccept: true;
		on: aModel
		text: getSel
		accept: setSel
		readSelection: nil
		menu: nil;
		acceptOnCR: true;
		getEnabledSelector: enabledSel;
		font: self textFont;
		cornerStyle: (self textEntryCornerStyleIn: aThemedMorph);
		hResizing: #spaceFill;
		vResizing: #rigid;
		borderStyle: (BorderStyle inset width: 1);
		"color: Color darkGray;
		selectionColor: self selectionColor;"
		hideScrollBarsIndefinitely;
		extent: 24@(self textFont height + 8);
		setBalloonText: helpText;
		entryCompletion: anEntryCompletion.

	pt textMorph
		autoFit: true;
		wrapFlag: false;
		margins: (2@1 corner: 2@1).
	^pt