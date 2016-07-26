new selection
highlightInterval: anInterval

	self textArea 
		ensureAndGetNewHighlightSegmentFrom: anInterval first 
		to: anInterval last + 1.