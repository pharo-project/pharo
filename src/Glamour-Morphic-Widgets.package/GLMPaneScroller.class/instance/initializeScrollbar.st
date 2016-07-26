initialization
initializeScrollbar
	scrollBar := ScrollBar new 
		model: self;
		setValueSelector: #scrollBarValue:;
		yourself.
	scrollBar 
		borderWidth: 0;
		borderColor: #inset;
		height: self scrollBarHeight.
	self resizeScrollBar.
