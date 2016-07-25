examples
constructFibonacciBlockInDeadFrame

	| fib |
	fib := [:val | 
		(val <= 0) ifTrue: [self error: 'not a natural number'].
		(val <= 2) ifTrue: [1]
		     ifFalse: [(fib value: (val - 1)) + (fib value: (val - 2))]].
	^fib
