manage scrolling
scroll: anEvent
	| newCenter |
	newCenter := circleCenter
		+ (anEvent direction abs * (anEvent dDist * (window extent distanceTo: 0 @ 0))).
	(newCenter < window extent and: [ newCenter > (0 @ 0) ])
		ifTrue: [ circleCenter := newCenter ]