tests
testFlush
	| q v sema |
	q := self newQueue.
	sema := Semaphore new.
	
	[ #( 1 2 3 4 5 6 7 8 9 10 ) do: [:each |
		q nextPut: each ]. sema signal ] fork.
	
	v := 0.
	
	sema wait. 
	q flush: [:each | self assert: v < each. v := each ].

	self assert: v = 10	