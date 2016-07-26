label-brick-accessing
editBrick: aBrick

	editBrick := aBrick.
	
	self editBrick
		hShrinkWrap;
		vShrinkWrap;
		withoutMargins;
		withoutAdornment;
		withoutBorder;
		beTransparent;
		autoAccept: true;
		rubricTextFamilyName: self themer textFamilyName;
		rubricTextFontSize: self themer textFontSize.
	
	self editBrick rubric forceHeight: [ self labelBrick height ].
	
	self editBrick rubric announcer when: RubReturnEntered  send: #onTextAccepted: to: self.
	self editBrick rubric announcer when: MorphLostFocus send: #onLostFocus to: self.
	self editBrick rubric announcer when: MorphGotFocus send: #onGotFocus: to: self.
	self editBrick rubric announcer when: RubTextAcceptRequest send: #onTextChanged: to: self