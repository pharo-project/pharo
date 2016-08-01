initialization
initializeWidgets
	button1 := self instantiate: RadioButtonModel.
	button2 := self instantiate: RadioButtonModel.
	button3 := self instantiate: RadioButtonModel.
	button1 label: 'Button 1'.
	button2 label: 'Button 2'.
	button3 label: 'Button 3'.
	self setFocus.
	self setGroup