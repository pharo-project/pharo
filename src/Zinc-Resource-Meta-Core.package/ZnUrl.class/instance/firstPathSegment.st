accessing-path
firstPathSegment
	^ self isSlash
		ifTrue: [ nil ]
		ifFalse: [ segments first ]