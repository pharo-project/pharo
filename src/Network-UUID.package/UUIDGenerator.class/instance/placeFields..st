instance creation
placeFields: aByteArray

	aByteArray at: 1 put: ((timeLow bitShift: -24) bitAnd: 16rFF).
	aByteArray at: 2 put: ((timeLow bitShift: -16) bitAnd: 16rFF).
	aByteArray at: 3 put: ((timeLow bitShift: -8) bitAnd: 16rFF).
	aByteArray at: 4 put: (timeLow bitAnd: 16rFF).
	aByteArray at: 5 put: ((timeMid bitShift: -8) bitAnd: 16rFF).
	aByteArray at: 6 put: (timeMid bitAnd: 16rFF).
	aByteArray at: 7 put: ((timeHiAndVersion bitShift: -8) bitAnd: 16rFF).
	aByteArray at: 8 put: (timeHiAndVersion bitAnd: 16rFF).
	aByteArray at: 9 put: clockSeqHiAndReserved.
	aByteArray at: 10 put: clockSeqLow.
	0 to: 5 do: [:i |
		aByteArray at: 11 + i put: ((node bitShift: (-8*i)) bitAnd: 16rFF)]
