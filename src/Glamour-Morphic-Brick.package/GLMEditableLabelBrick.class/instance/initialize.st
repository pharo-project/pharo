initialization
initialize
	super initialize.
	
	self labelBrick: self newLabelBrick.
	self editBrick: self newEditBrick.
	
	self
		padding: self themer focusedBorderWidth;
		borderWidth: self themer focusedBorderWidth;
		hShrinkWrap;
		vShrinkWrap;
		enable;
		enableDoubleClick;
		when: #onDoubleClicked do: [ (self hasSubbrick: self editBrick) ifFalse: [ self beEdit ] ];
		bindKeyCombination: Character escape toAction: [ self onEscapePressed ]