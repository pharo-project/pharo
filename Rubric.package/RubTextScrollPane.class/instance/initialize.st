initialize
initialize
	super initialize.
	self scroller extent: self textArea extent.
	self wrapped ifTrue: [ self fitScrollTarget  ].
	drawFocusBorder := true