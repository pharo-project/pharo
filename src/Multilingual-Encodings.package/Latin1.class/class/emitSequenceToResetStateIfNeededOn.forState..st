class methods
emitSequenceToResetStateIfNeededOn: aStream forState: state 
	state g0Leading ~= 0 ifTrue: 
		[ state charSize: 1.
		state g0Leading: 0.
		state g0Size: 1.
		aStream basicNextPutAll: compoundTextSequence ]

	"Actually, G1 state should go back to ISO-8859-1, too."