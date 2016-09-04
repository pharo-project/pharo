accessing - data
int16: anInteger
	"Store the given signed, 16-bit integer on this (binary) stream."

	| n |
	(anInteger < -16r8000 or: [ anInteger >= 16r8000 ])
		ifTrue: [ self error: 'outside 16-bit integer range' ].
	n := anInteger < 0
		ifTrue: [ 16r10000 + anInteger ]
		ifFalse: [ anInteger ].
	self nextPut: (n digitAt: 2).
	self nextPut: (n digitAt: 1)