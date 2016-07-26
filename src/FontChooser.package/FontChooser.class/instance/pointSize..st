accessing
pointSize: aNumber

	pointSize := aNumber.
	self announcer announce: #pointSizeChanged.