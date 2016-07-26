examples
constructFibonacciBlockWithBlockArgumentInDeadFrame

	^ [:val :blk | 
		(val <= 0) ifTrue: [self error: 'not a natural number'].
		(val <= 2) ifTrue: [1]
		    ifFalse: [(blk value: (val - 1) value: blk) + (blk value: (val - 2) value: blk)]].
