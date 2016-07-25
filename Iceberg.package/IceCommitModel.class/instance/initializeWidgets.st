initialization
initializeWidgets
	message := self newText.
	commitButton := self newButton 
		label: 'Commit';
		yourself.
		
	refreshButton := self newButton
		label: 'Refresh changes';
		yourself.
	
	self focusOrder 
		add: message.