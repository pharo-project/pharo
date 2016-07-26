initialization
initialize
	super initialize.
	
	self
		checkBrick: self newCheckboxBrick;
		labelBrick: self newLabelBrick;
		useHorizontalLinearLayout;
		vShrinkWrap;
		hShrinkWrap;
		enable