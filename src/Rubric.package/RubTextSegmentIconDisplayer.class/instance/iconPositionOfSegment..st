drawing
iconPositionOfSegment: aSegment
	^ (self left + self horizontalGapBefore) @ aSegment position y
		translateBy: 0 negated @ (self offset y negated + self bounds top)