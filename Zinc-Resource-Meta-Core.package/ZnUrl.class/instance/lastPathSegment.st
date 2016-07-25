accessing-path
lastPathSegment
	^ self isSlash
		ifTrue: [ nil ]
		ifFalse: [ segments last ]