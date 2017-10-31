tests
testReentrantBlockOldEnvironment

	| fib |
	fib := self constructFibonacciBlockInDeadFrame.
	self 
		should: [fib value: 0] 
		raise: self classForTestResult error.
	self assert: ((fib value: 1) = 1).
	self assert: ((fib value: 2) = 1).
	self assert: ((fib value: 3) = 2).
	self assert: ((fib value: 4) = 3).
	self assert: ((fib value: 5) = 5).
	self assert: ((fib value: 6) = 8).
