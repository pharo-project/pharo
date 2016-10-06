new selection
hoverHighlightInterval: anInterval

	self textArea 
		ensureAndGetNewHoverHighlightSegmentFrom: anInterval first 
		to: anInterval last + 1.