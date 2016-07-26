class methods
nextPutValue: ascii toStream: aStream withShiftSequenceIfNeededForTextConverterState: state 
	(ascii <= 127 and: [ state g0Leading ~= 0 ]) ifTrue: 
		[ state charSize: 1.
		state g0Leading: 0.
		state g0Size: 1.
		aStream basicNextPutAll: compoundTextSequence.
		aStream basicNextPut: (Character value: ascii).
		^ self ].
	((128 <= ascii and: [ ascii <= 255 ]) and: [ state g1Leading ~= 0 ]) ifTrue: 
		[ ^ self 
			nextPutRightHalfValue: ascii
			toStream: aStream
			withShiftSequenceIfNeededForTextConverterState: state ].
	aStream basicNextPut: (Character value: ascii).
	^ self