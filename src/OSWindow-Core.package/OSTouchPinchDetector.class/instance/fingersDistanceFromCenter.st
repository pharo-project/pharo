preconditions
fingersDistanceFromCenter
	| centerOfAllFingers |
	centerOfAllFingers := self centerOfAllFingers.
	^ (self activeFingers collect: [ :each | centerOfAllFingers distanceTo: each lastPosition ]) sum