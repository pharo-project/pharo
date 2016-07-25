preconditions
eachFingersHadTravelledEnough
	^ self distanceTravelledForEachFingers allSatisfy: [ :each | each > minDistBeforeDetection ]