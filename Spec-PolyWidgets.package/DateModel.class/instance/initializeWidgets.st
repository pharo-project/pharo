initialization
initializeWidgets

	dateLabel := self newLabel.
	dateModel := self newTextInput.
	chooseDateButton := self newButton.
		
	dateLabel
		label: 'Date'.
		
	dateModel
		text: Date today asString;
		isCodeCompletionAllowed: false.
	
	chooseDateButton
		icon: self iconMorph;
		action: [ self chooseDate ];
		extent: self iconMorph extent.