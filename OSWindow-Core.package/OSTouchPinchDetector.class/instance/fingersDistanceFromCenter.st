preconditions
fingersDistanceFromCenter
	| centerOfAllFingers |
	centerOfAllFingers := self centerOfAllFingers.
	^ (self activeFingers collect: [ :each | centerOfAllFingers dist: each lastPosition ]) sum