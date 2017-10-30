initialization
initializeWidgets

	dateLabel := self newLabel.
	dateModel := self newTextInput.
	chooseDateButton := self newButton.
	
	dateLabel
		label: 'Date'.
		
	dateModel
		isCodeCompletionAllowed: false.
	
	chooseDateButton
		icon: self iconMorph;
		action: [ self chooseDate ];
		extent: self iconMorph extent.