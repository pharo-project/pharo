initialization
initialize
	super initialize.
		
	resizingZoneWidth := 7.
	isRightResizing := false.
	isLeftResizing := false.
	isResizing := false.
		
	self
		normalColor: (Color veryLightGray alpha: 0.4);
		selectedColor: (Color veryLightGray alpha: 0.55);
		enable