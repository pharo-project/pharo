Extension { #name : 'IntegerTest' }

{ #category : '*System-Benchmark-Tests' }
IntegerTest >> testBenchFib [
	self assert: 0 benchFib equals: 1.
	self assert: 1 benchFib equals: 1.
	self assert: 2 benchFib equals: 3
]
