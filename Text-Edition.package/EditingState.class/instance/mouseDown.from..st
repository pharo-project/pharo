events
mouseDown: anEvent from: aTextMorph
	self mouseDownInterval: aTextMorph editor selectionInterval.
	self focused: true from: aTextMorph
