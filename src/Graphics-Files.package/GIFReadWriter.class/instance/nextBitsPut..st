private-bits access
nextBitsPut: anInteger 
	| integer writeBitCount shiftCount |
	shiftCount := 0.
	remainBitCount = 0 
		ifTrue: 
			[ writeBitCount := 8.
			integer := anInteger ]
		ifFalse: 
			[ writeBitCount := remainBitCount.
			integer := bufByte + (anInteger bitShift: 8 - remainBitCount) ].
	[ writeBitCount < codeSize ] whileTrue: 
		[ self nextBytePut: ((integer bitShift: shiftCount) bitAnd: 255).
		shiftCount := shiftCount - 8.
		writeBitCount := writeBitCount + 8 ].
	(remainBitCount := writeBitCount - codeSize) = 0 
		ifTrue: [ self nextBytePut: (integer bitShift: shiftCount) ]
		ifFalse: [ bufByte := integer bitShift: shiftCount ].
	^ anInteger