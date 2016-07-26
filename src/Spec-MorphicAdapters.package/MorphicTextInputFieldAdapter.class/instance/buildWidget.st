factory
buildWidget
	| plu |
	plu := PluggableTextFieldMorph new
		convertTo: String;
		on: self
			text: #getText
			accept: #accept:
			readSelection: nil
			menu: nil.
	plu
		entryCompletion: self entryCompletion;
		autoAccept: self autoAccept;
		ghostText: self ghostText;
		enabled: self enabled;
		encrypted: self encrypted;
		setBalloonText: self help;
		dragEnabled: self dragEnabled;
		dropEnabled: self dropEnabled;
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		acceptOnCR: self acceptOnCR;
		hideScrollBarsIndefinitely.
	^ plu