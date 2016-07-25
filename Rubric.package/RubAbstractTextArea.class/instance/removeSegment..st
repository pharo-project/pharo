accessing segments
removeSegment: aRubTextSegmentMorph
	self announcer unsubscribe: aRubTextSegmentMorph.
	self segments remove: aRubTextSegmentMorph ifAbsent: [].
