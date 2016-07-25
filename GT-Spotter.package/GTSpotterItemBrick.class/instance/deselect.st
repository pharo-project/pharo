event handling
deselect
	super deselect.
	self isChecked ifFalse: [ self hideActionbar ]