converting
visitQuadSegment: quadSegment

	self duplicateLastPoint.

	contours last add: quadSegment via;
		add: quadSegment to