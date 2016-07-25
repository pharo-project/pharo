initialization
initializeWidgets

	button := self newButton.
	bottom := self newList.
		
	button 
		label: 'Change';
		action: [ self change ];
		state: false.
		
	bottom 
		items: (1 to: 10) asOrderedCollection.