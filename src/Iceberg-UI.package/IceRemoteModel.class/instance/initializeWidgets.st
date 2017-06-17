initialization
initializeWidgets
	nameLabel := self newLabel. 
	namePanel := self newTextInput ghostText: 'user-remote'; autoAccept: true.
	urlLabel := self newLabel.
	urlPanel := self newTextInput ghostText: 'git@github.com:user/project.git'; autoAccept: true.
	addButton := self newButton.
	
	nameLabel label: 'Remote name'.
	urlLabel label: 'Remote URL'.
	addButton label: 'Add remote'.
	addButton action: [ self accept ].
	
	self focusOrder 
		add: namePanel;
		add: urlPanel;
		add: addButton
