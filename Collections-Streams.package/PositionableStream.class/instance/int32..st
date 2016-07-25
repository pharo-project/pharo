data get/put
int32: anInteger
	"Store the given signed, 32-bit integer on this (binary) stream."

	| n |
	(anInteger < -16r80000000 or: [ anInteger >= 16r80000000 ])
		ifTrue: [ self error: 'outside 32-bit integer range' ].
	n := anInteger < 0
		ifTrue: [ 16r100000000 + anInteger ]
		ifFalse: [ anInteger ].
	self nextPut: (n digitAt: 4).
	self nextPut: (n digitAt: 3).
	self nextPut: (n digitAt: 2).
	self nextPut: (n digitAt: 1)