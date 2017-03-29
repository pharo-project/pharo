events-processing
onOpened
	super onOpened.

	self textEditBrick
		takeKeyboardFocus;
		selectAll