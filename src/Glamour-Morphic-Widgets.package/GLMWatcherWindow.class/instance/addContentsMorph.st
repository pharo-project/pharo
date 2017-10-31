initialize
addContentsMorph
	self contentsMorph: GLMMorphic emptyMorph.
	self contentsMorph
		layoutFrame: (LayoutFrame identity 
							leftOffset: 10; 
							topOffset: 10; 
							rightOffset: -10;
							bottomOffset: -10);
		hResizing: #spaceFill;
		vResizing: #spaceFill.
	self addMorphBack: self contentsMorph