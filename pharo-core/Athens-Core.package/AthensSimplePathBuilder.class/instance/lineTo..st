path commands
lineTo: aPoint

	^ self addSegment: (AthensLineSegment new point: (self toAbsolute: aPoint)).

