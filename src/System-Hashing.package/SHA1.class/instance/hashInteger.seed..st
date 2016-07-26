accessing
hashInteger: aPositiveInteger seed: seedInteger 
	"Hash the given positive integer. The integer to be hashed should have 512 or fewer bits. This entry point is used in the production of random numbers"
	"Initialize totalA through totalE to their seed values."
	| buffer dstIndex |
	totalA := ThirtyTwoBitRegister new load: ((seedInteger bitShift: -128) bitAnd: 4294967295).
	totalB := ThirtyTwoBitRegister new load: ((seedInteger bitShift: -96) bitAnd: 4294967295).
	totalC := ThirtyTwoBitRegister new load: ((seedInteger bitShift: -64) bitAnd: 4294967295).
	totalD := ThirtyTwoBitRegister new load: ((seedInteger bitShift: -32) bitAnd: 4294967295).
	totalE := ThirtyTwoBitRegister new load: (seedInteger bitAnd: 4294967295).
	self initializeTotalsArray.

	"pad integer with zeros"
	buffer := ByteArray new: 64.
	dstIndex := 0.
	aPositiveInteger digitLength 
		to: 1
		by: -1
		do: 
			[ :i | 
			buffer 
				at: (dstIndex := dstIndex + 1)
				put: (aPositiveInteger digitAt: i) ].

	"process that one block"
	self processBuffer: buffer.
	^ self finalHash