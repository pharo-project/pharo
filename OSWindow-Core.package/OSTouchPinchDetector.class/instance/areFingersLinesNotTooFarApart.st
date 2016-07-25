preconditions
areFingersLinesNotTooFarApart
	| firstFinger secondFinger minDistance |
	firstFinger := self activeFingers first.
	secondFinger := self activeFingers last.
	minDistance := (firstFinger distanceFromLineFormedByFinger: secondFinger)
		min: (secondFinger distanceFromLineFormedByFinger: firstFinger).
	^ minDistance < maxDistanceBetweenFingersLine