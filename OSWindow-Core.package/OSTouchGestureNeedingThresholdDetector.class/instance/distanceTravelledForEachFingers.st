calculating
distanceTravelledForEachFingers
	^ self activeFingers
			collect: [ :finger | finger distanceTravelled ]