highlighting
ensureAndGetNewHoverHighlightSegmentFrom: startIndex to: stopIndex
	
	^ self 
		removeHoverHighlightSegment;
		addSegment: (RubHoverHighlightSegmentMorph from: startIndex to: stopIndex)