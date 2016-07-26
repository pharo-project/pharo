positioning
match: subCollection 
	"Set the access position of the receiver to be past the next occurrence of the subCollection. Answer whether subCollection is found.  No wildcards, and case does matter."
	| pattern startMatch |
	pattern := subCollection readStream.
	startMatch := nil.
	[ pattern atEnd ] whileFalse: 
		[ self atEnd ifTrue: [ ^ false ].
		self next = pattern next 
			ifTrue: [ pattern position = 1 ifTrue: [ startMatch := self position ] ]
			ifFalse: 
				[ pattern position: 0.
				startMatch ifNotNil: 
					[ self position: startMatch.
					startMatch := nil ] ] ].
	^ true