initialization
initializeWidgets
	message := self newText
		autoAccept: true;
		yourself.
		
	commitButton := self newButton
		label: 'Commit';
		yourself.
	commitPushButton := self newButton
		label: 'Commit and push';
		yourself.
		
	self focusOrder add: message