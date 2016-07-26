selected
selectedPointSizeIndex: anIndex

	anIndex = 0 ifTrue: [^self].
	pointSize := (self pointSizeList at: anIndex) trimBoth asNumber.
	self announcer announce: #pointSizeChanged.
	