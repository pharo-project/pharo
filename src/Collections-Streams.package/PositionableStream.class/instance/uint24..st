accessing - data
uint24: anInteger
	"Store the given unsigned, 24-bit integer on this (binary) stream."

	(anInteger < 0 or: [ anInteger >= 16r1000000 ])
		ifTrue: [self error: 'outside unsigned 24-bit integer range'].

	self nextPut: (anInteger digitAt: 3).
	self nextPut: (anInteger digitAt: 2).
	self nextPut: (anInteger digitAt: 1).
