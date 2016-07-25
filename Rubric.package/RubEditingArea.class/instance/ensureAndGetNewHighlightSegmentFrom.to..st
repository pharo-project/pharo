highlighting
ensureAndGetNewHighlightSegmentFrom: startIndex to: stopIndex
	
	^ self 
		removeHighlightSegment;
		addSegment: (RubHighlightSegmentMorph from: startIndex to: stopIndex)