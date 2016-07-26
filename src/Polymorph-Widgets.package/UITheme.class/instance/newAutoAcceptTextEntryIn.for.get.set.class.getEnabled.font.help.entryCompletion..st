morph creation
newAutoAcceptTextEntryIn: aThemedMorph for: aModel  get: getSel set: setSel class: aClass getEnabled: enabledSel font: aFont help: helpText entryCompletion: anEntryCompletion
	"Answer a text entry for the given model.
	Object conversion will be automatic based on the class of
	object returned after performing the get selector, aClass
	is provided in case of initial nil returns."

	|pt|
	pt := PluggableTextFieldMorph new
		autoAccept: true;
		alwaysAccept: true;
		convertTo: aClass;
		on: aModel
		text: getSel
		accept: setSel
		readSelection: nil
		menu: nil;
		acceptOnCR: true;
		getEnabledSelector: enabledSel;
		font: aFont;
		cornerStyle: (self textEntryCornerStyleIn: aThemedMorph);
		hResizing: #spaceFill;
		vResizing: #rigid;
		borderStyle: (BorderStyle inset width: 1);
		selectionColor: self selectionColor;
		hideScrollBarsIndefinitely;
		extent: 24@(aFont height + 8);
		setBalloonText: helpText;
		entryCompletion: anEntryCompletion.
	pt textMorph
		autoFit: true;
		wrapFlag: false;
		margins: (2@1 corner: 2@1).
	^pt
