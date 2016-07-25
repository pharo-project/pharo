scrolling
scrollAbsolute: event
	| r p |
	r := self roomToMove.
	bounds isWide
		ifTrue: [r width = 0 ifTrue: [^ self]]
		ifFalse: [r height = 0 ifTrue: [^ self]].
	p := event targetPoint adhereTo: r.
	self descending
		ifFalse:
			[self setValue: (bounds isWide 
				ifTrue: [(p x - r left) asFloat / r width]
				ifFalse: [(p y - r top) asFloat / r height])]
		ifTrue:
			[self setValue: (bounds isWide
				ifTrue: [(r right - p x) asFloat / r width]
				ifFalse:	[(r bottom - p y) asFloat / r height])]