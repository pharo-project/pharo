highlighting
getHighlightSegment

	^ self segments detect: [ :each | each class = RubHighlightSegmentMorph ] ifNone: [ nil ]