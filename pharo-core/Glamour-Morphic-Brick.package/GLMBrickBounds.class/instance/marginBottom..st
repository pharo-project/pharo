accessing - margin/padding
marginBottom: anInteger

	self translateY: self marginBottom - anInteger.
	self margin at: Bottom put: anInteger.
	self updateCache