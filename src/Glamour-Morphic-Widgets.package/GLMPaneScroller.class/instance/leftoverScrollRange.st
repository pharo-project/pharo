scrolling
leftoverScrollRange
	^ (self totalScrollRange - self innerBounds width roundTo: self scrollDeltaWidth) max: 0
