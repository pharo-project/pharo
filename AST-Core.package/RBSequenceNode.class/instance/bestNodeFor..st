querying
bestNodeFor: anInterval 
	| node |
	node := super bestNodeFor: anInterval.
	node == self 
		ifTrue: 
			[(temporaries isEmpty and: [statements size == 1]) 
				ifTrue: [^statements first]].
	^node