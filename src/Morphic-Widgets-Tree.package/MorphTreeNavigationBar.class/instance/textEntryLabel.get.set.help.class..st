accessing
textEntryLabel: aLabel get: getSel set: setSel help: helpText class: aClass
 	| pt |
	pt := PluggableTextFieldMorph new
		convertTo: aClass;
		alwaysAccept: true;
		on: self
		text: getSel
		accept: setSel
		readSelection: nil
		menu: nil;
		acceptOnCR: true;
		getEnabledSelector: nil;
		font: self preferedFont;
		cornerStyle: #square;
		hResizing: #spaceFill;
		vResizing: #rigid;
		borderStyle: (BorderStyle inset width: 1);
		color: Color white;
		hideScrollBarsIndefinitely;
		extent: 24@ (self preferedFont height + 4);
		setBalloonText: helpText.
	pt textMorph
		autoFit: true;
		wrapFlag: false;
		margins: (1@0 corner: 1@0).
	^pt