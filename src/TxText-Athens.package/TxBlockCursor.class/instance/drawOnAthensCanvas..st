drawing
drawOnAthensCanvas: aCanvas

	self flag: 'Most of this object is currently just copied from TxCaretCursor'.
	self isBlinkingVisibleNow ifTrue: [ 
		aCanvas 
			setShape: self calculateNewBounds;
			setPaint: self cursorColor;
			draw
		]