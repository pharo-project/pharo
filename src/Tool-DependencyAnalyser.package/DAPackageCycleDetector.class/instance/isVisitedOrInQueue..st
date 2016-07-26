testing
isVisitedOrInQueue: aNode
	^ (visitedNodes includes: aNode) or: [ aNode isInStack ]