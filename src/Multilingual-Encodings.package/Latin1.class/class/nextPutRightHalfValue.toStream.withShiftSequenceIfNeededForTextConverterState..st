private
nextPutRightHalfValue: ascii toStream: aStream withShiftSequenceIfNeededForTextConverterState: state 
	state charSize: 1.
	state g1Leading: 0.
	state g1Size: 1.
	aStream basicNextPutAll: rightHalfSequence.
	aStream basicNextPut: (Character value: ascii)