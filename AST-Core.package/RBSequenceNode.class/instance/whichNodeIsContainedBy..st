querying
whichNodeIsContainedBy: anInterval 
	| node |
	node := super whichNodeIsContainedBy: anInterval.
	node == self 
		ifTrue: 
			[(temporaries isEmpty and: [statements size == 1]) 
				ifTrue: [^statements first]].
	^node