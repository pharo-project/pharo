stream access
nextWordPut: a16BitW
	"Write out a 16-bit integer as 16 bits."

	stream nextPut: ((a16BitW bitShift: -8) bitAnd: 16rFF).
	stream nextPut: (a16BitW bitAnd: 16rFF).
	^a16BitW