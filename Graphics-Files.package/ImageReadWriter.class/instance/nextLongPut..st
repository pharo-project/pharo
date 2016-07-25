stream access
nextLongPut: a32BitW
	"Write out a 32-bit integer as 32 bits."

	stream nextPut: ((a32BitW bitShift: -24) bitAnd: 16rFF).
	stream nextPut: ((a32BitW bitShift: -16) bitAnd: 16rFF).
	stream nextPut: ((a32BitW bitShift: -8) bitAnd: 16rFF).
	stream nextPut: (a32BitW bitAnd: 16rFF).
	^a32BitW