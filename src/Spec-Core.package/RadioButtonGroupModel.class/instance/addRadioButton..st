protocol
addRadioButton: aButton
	self linkRadioButton: aButton.
	aButton canDeselectByClick: self canDeselectByClick.
	buttons add: aButton.
	aButton privateSetState: false;
		owner: self.
	self focusOrder add: aButton ..
	self rebuildWidget