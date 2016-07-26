converting
reversed
	^ self copy
		startOffset: startDelta negated;
		endOffset: endDelta negated;
		yourself