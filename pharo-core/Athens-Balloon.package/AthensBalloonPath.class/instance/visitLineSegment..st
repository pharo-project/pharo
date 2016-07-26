converting
visitLineSegment: line
	self duplicateLastPoint.
	contours last add: line endPoint;
		add: line endPoint.