updating screen
updateRectangle: aRectangle
	self primShowRectLeft: aRectangle left truncated
		right: aRectangle right rounded
		top: aRectangle top truncated
		bottom: aRectangle bottom rounded.
