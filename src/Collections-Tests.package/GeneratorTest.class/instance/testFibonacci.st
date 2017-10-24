tests
testFibonacci
	| generator |
	generator := self fibonacciSequence.
	self assert: (generator next: 10) asArray = #( 1 1 2 3 5 8 13 21 34 55 )