class methods
nextPutValue: ascii toStream: aStream withShiftSequenceIfNeededForTextConverterState: state 
	| c1 c2 |
	state charSize: 2.
	state g0Leading ~= self leadingChar ifTrue: 
		[ state g0Leading: self leadingChar.
		state g0Size: 2.
		aStream basicNextPutAll: compoundTextSequence ].
	c1 := ascii // 94 + 33.
	c2 := ascii \\ 94 + 33.
	^ aStream
		basicNextPut: (Character value: c1);
		basicNextPut: (Character value: c2)