accessing-path
removeLastPathSegment
	self hasPath
		ifTrue: [ segments removeLast ]