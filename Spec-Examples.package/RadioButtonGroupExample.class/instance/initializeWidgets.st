initialization
initializeWidgets

	button1 := self newRadioButton.
	button2 := self newRadioButton.
	button3 := self newRadioButton.
	
	button1 label: 'Button 1'.
	button2 label: 'Button 2'.
	button3 label: 'Button 3'.
	
	self setFocus.
	self setGroup.
	