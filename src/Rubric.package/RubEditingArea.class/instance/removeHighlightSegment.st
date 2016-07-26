highlighting
removeHighlightSegment

	(self getHighlightSegment)
		ifNotNil: [ :aSegment | aSegment delete ].