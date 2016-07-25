accessing
hashInteger: aPositiveInteger 
	"Hash the given positive integer. The integer to be hashed should have 512 or fewer bits. This entry point is used in key generation."
	| buffer dstIndex |
	self initializeTotals.

	"pad integer with zeros"
	aPositiveInteger highBit <= 512 ifFalse: [ self error: 'integer cannot exceed 512 bits' ].
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