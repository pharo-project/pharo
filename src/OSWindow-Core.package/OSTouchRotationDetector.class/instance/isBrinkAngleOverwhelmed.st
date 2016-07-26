preconditions
isBrinkAngleOverwhelmed
	^ (currentAngle > (2 * Float pi - minAngleBeforeDetection) or: [ currentAngle < minAngleBeforeDetection ]) not