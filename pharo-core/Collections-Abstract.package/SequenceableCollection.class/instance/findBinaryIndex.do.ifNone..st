enumerating
findBinaryIndex: aBlock do: actionBlock ifNone: exceptionBlock
	"Search for an element in the receiver using binary search.
	The argument aBlock is a one-element block returning
		0 	- if the element is the one searched for
		<0	- if the search should continue in the first half
		>0	- if the search should continue in the second half
	If found, evaluate actionBlock with the index as argument
	If no matching element is found, evaluate exceptionBlock,
	with the indexes of the 'bounding' elements as arguments.
	Warning: Might give invalid indexes, see examples below
	Examples:
		#(1 3 5 7 11 15 23)
			findBinaryIndex: [ :arg | 11 - arg ]
			do: [ :found | found ]
			ifNone: [ :a :b | 'between: ', {a. b} printString ]
		#(1 3 5 7 11 15 23)
			findBinaryIndex: [ :arg | 12 - arg ]
			do: [ :found | found ]
			ifNone: [ :a :b | 'between: ', {a. b} printString ]
		#(1 3 5 7 11 15 23)
			findBinaryIndex: [ :arg | 0.5 - arg ]
			do: [ :found | found ]
			ifNone: [ :a :b | 'between: ', {a. b} printString ]
		#(1 3 5 7 11 15 23)
			findBinaryIndex: [ :arg | 25 - arg ]
			do: [ :found | found ]
			ifNone: [ :a :b | 'between: ',{a. b} printString ] "
			
	| index low high test |
	low := 1.
	high := self size.
	[ index := high + low // 2.
	  low > high ] whileFalse: [
		test := aBlock value: (self at: index).
		test = 0 
			ifTrue: [ ^ actionBlock value: index ]
			ifFalse: [ test > 0
				ifTrue: [ low := index + 1 ]
				ifFalse: [ high := index - 1 ] ] ].
	^ exceptionBlock cull: high cull: low