initialization
initializeWidgets 

	button1 := self newCheckBox.
	button2 := self newCheckBox.
	button3 := self newCheckBox.
	
	button1 label: 'Button 1'.
	button2 label: 'Button 2'.
	button3 label: 'Button 3'.
	self setFocus.