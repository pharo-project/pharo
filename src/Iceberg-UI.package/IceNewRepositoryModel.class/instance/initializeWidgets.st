initialization
initializeWidgets
	projectNameLabel := self newLabel label: 'Project name'.
	projectNameInput := self newTextInput 
		ghostText: 'project'; 
		autoAccept: true.
	self focusOrder add: projectNameInput.
	super initializeWidgets