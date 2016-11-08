initialization
initializeTextArea
	textArea 
		enabled: false;
		"make sure we can cycle over the text field"
		bindKeyCombination: Character tab toAction: [ self giveFocusToNextFrom: textArea ];
		bindKeyCombination: Character tab shift toAction: [ self giveFocusToPreviousFrom: textArea ].