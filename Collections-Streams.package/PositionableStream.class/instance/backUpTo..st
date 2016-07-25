positioning
backUpTo: subCollection 
	"Back up the position to he subCollection.  Position must be somewhere within the stream initially.  Leave it just after it.  Return true if succeeded.  No wildcards, and case does matter."
	"Example:
	| strm | strm := ReadStream on: 'zabc abdc'.
	strm setToEnd; backUpTo: 'abc'; position 
"
	| pattern startMatch |
	pattern := subCollection reversed readStream.
	startMatch := nil.
	[ pattern atEnd ] whileFalse: 
		[ self position = 0 ifTrue: [ ^ false ].
		self skip: -1.
		self next = pattern next 
			ifTrue: [ pattern position = 1 ifTrue: [ startMatch := self position ] ]
			ifFalse: 
				[ pattern position: 0.
				startMatch ifNotNil: 
					[ self position: startMatch - 1.
					startMatch := nil ] ].
		self skip: -1 ].
	self position: startMatch.
	^ true