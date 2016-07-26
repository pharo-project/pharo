converting
visitMoveSegment: seg
	
	contours ifNil: [
		contours := OrderedCollection new ].
	
	contours add: OrderedCollection new.
	contours last add: seg endPoint.