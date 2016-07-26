initialization
initializeScrollBars
	verticalScrollBar := ScrollBar new 
		model: self; 
		setValueSelector: #verticalScrollBarValue:;
		yourself.
	self addMorph: verticalScrollBar