calculating
distanceFromLineFormedByFinger: aFingerTracker
	| otherLast |
	otherLast := aFingerTracker lastPosition.

	^ lastPosition distanceTo: 
		(lastPosition nearestPointAlongLineFrom: 
			otherLast + aFingerTracker delta 
		to:
			otherLast)