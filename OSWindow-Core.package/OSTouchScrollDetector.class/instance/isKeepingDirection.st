preconditions
isKeepingDirection
	^ persistentDirection not
		or: [	directionAtBeginning = (0 @ 0)
		or: [ direction abs = directionAtBeginning abs ] ]