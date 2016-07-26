calculating
distanceFromLineFormedByFinger: aFingerTracker
	| otherLast |
	otherLast := aFingerTracker lastPosition.

	^ lastPosition dist: 
		(lastPosition nearestPointAlongLineFrom: 
			otherLast + aFingerTracker delta 
		to:
			otherLast)