data get/put
uint32: anInteger
	"Store the given unsigned, 32-bit integer on this (binary) stream."

	(anInteger < 0 or: [ anInteger >= 16r100000000 ])
		ifTrue: [self error: 'outside unsigned 32-bit integer range'].

	self nextPut: (anInteger digitAt: 4).
	self nextPut: (anInteger digitAt: 3).
	self nextPut: (anInteger digitAt: 2).
	self nextPut: (anInteger digitAt: 1).
