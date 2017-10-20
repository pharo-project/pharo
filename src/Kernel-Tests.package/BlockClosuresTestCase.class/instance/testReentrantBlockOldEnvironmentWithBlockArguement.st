tests
testReentrantBlockOldEnvironmentWithBlockArguement

	| fib |
	fib := self constructFibonacciBlockWithBlockArgumentInDeadFrame.
	self 
		should: [fib value: 0 value: fib] 
		raise: self classForTestResult error.
	self assert: ((fib value: 1 value: fib) = 1).
	self assert: ((fib value: 2 value: fib) = 1).
	self assert: ((fib value: 3 value: fib) = 2).
	self assert: ((fib value: 4 value: fib) = 3).
	self assert: ((fib value: 5 value: fib) = 5).
	self assert: ((fib value: 6 value: fib) = 8).
