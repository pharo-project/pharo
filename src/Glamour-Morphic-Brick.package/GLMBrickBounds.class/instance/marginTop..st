accessing - margin/padding
marginTop: anInteger

	self translateY: anInteger - self marginTop.
	self margin at: Top put: anInteger.
	self updateCache