tests
testReentrantBlock

	| fib |
	fib := [:val | 
		(val <= 0) ifTrue: [self error: 'not a natural number'].
		(val <= 2) 
			ifTrue: [1]
			ifFalse: [(fib value: (val - 1)) + (fib value: (val - 2))]].

	self 
		should: [fib value: 0] 
		raise: self classForTestResult error.
	self assert: ((fib value: 1) = 1).
	self assert: ((fib value: 2) = 1).
	self assert: ((fib value: 3) = 2).
	self assert: ((fib value: 4) = 3).
	self assert: ((fib value: 5) = 5).
	self assert: ((fib value: 6) = 8).
