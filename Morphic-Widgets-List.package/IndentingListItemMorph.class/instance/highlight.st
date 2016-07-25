private-container protocol
highlight

	complexContents highlightingColor ifNotNil: [self color: complexContents highlightingColor].
	self changed.
	
