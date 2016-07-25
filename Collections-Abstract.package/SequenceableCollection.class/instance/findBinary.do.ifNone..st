enumerating
findBinary: aBlock do: actionBlock ifNone: exceptionBlock
	"Search for an element in the receiver using binary search.
	The argument aBlock is a one-element block returning
		0 	- if the element is the one searched for
		<0	- if the search should continue in the first half
		>0	- if the search should continue in the second half
	If found, evaluate actionBlock with the found element as argument
	If no matching element is found, evaluate exceptionBlock,
	with the 'bounding' elements or nil as arguments.
	Examples:
		#(1 3 5 7 11 15 23)
			findBinary: [ :arg | 11 - arg ]
			do: [ :found | found ]
			ifNone: [ :a :b | 'between: ', {a. b} printString ]
		#(1 3 5 7 11 15 23)
			findBinary: [ :arg | 12 - arg ]
			do: [ :found | found ]
			ifNone: [ :a :b | 'between: ', {a. b} printString ]
		#(1 3 5 7 11 15 23)
			findBinary: [ :arg | 0.5 - arg ]
			do: [ :found | found ]
			ifNone: [ :a :b | 'between: ', {a. b} printString ]
		#(1 3 5 7 11 15 23)
			findBinary: [ :arg | 25 - arg ]
			do: [ :found | found ]
			ifNone: [ :a :b | 'between: ',{a. b} printString ] "
	
	^ self
		findBinaryIndex: aBlock
		do: [ :foundIndex |
			actionBlock value: (self at: foundIndex) ]
		ifNone: [ :prevIndex :nextIndex |
			exceptionBlock
				cull: (prevIndex > 0 ifTrue: [ self at: prevIndex ])
				cull: (nextIndex <= self size ifTrue: [ self at: nextIndex ]) ]
