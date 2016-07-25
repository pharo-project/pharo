accessing
target: aSystemWindow 
	target := aSystemWindow.
	self color: (target paneColor alpha: 0.35).
	self bounds: aSystemWindow bounds.
	self currentHand newMouseFocus: self