Extension { #name : 'NumberTest' }

{ #category : '*Kernel-Extended-Tests' }
NumberTest >> testPercent [
	self assert: 20 / 40 equals: 50 percent
]
