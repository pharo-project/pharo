Extension { #name : 'BlockClosuresTestCase' }

{ #category : '*Kernel-Extended-Tests' }
BlockClosuresTestCase >> testExample1 [
	self assert: (self example1: 5) equals: 5 factorial
]
