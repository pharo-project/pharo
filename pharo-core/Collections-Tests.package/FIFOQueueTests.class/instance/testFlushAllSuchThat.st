tests
testFlushAllSuchThat
	| q |
	q := self newQueue.

	
	#( 1 2 3 4 5 6 7 8 9 10 ) do: [:each | q nextPut: each ]. 
	
	q flushAllSuchThat: [:each | each odd ].
	
	q flush: [:each | self assert: each even ].
	
	self assert: q nextOrNil == nil