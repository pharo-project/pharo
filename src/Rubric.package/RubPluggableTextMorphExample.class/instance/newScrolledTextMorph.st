view creation
newScrolledTextMorph
	| st |
	st := RubPluggableTextMorph new
		on: self
			text: #getText
			accept: #accept:notifying:
			readSelection: #readSelection
			menu: nil
			setSelection: #setSelection:;
		getEnabledSelector: #enabled;
		askBeforeDiscardingEdits: self askBeforeDiscardingEdits;
		autoAccept: self autoAccept;
		setBalloonText: self help;
		dragEnabled: self dragEnabled;
		dropEnabled: self dropEnabled;
		registerScrollChanges: #scrollValueChanged:;
		vResizing: #spaceFill;
		hResizing: #spaceFill;
		yourself.
	st
		beWrapped;
		autoAccept: false;
		beForSmalltalkCode;
		withCodeSizeFeedback.
	^ st