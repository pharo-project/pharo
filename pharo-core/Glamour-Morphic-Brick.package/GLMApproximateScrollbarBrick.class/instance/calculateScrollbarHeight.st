scrollbar-calculations
calculateScrollbarHeight
	|bandHeight firstIndex lastIndex |
	bandHeight := self approximateBandHeight.
	bandHeight = 0 ifTrue: [ ^ 0 ].
	self scrollPane isFullyVisible ifTrue: [ ^ 0 ].
	
	firstIndex := self scrollPane getIndex: (self scrollPane band firstSubbrick).
	lastIndex := self scrollPane getIndex: (self scrollPane band lastSubbrick).
	
	^ ((lastIndex - firstIndex + 1) / self scrollPane model count * self height) ceiling.
	