actions
findAllCyclesFrom: stronglyConnectedComponents for: package
	| parents |
	self initializeVisitedNodes.
	parents := self parentsFrom: stronglyConnectedComponents For: package.
	self initializeQueueWith: package.
	 [ parents isEmpty ] whileFalse: [ self startBfsFrom: stronglyConnectedComponents For: parents ]