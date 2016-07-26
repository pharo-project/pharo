tests
testSize
	| q |
	q := self newQueue.
	
	#( 1 2 3 4 5 6 7 8 9 10 ) do: [:each |
		q nextPut: each ].

	self assert: (q size = 10)