instance creation
newLabelBrick
	self flag: 'find a better number, or at least a better place for the number :)'.

	^ GLMLabelBrick new 
		text: 'Search result with very very long name';
		paddingRight: 16