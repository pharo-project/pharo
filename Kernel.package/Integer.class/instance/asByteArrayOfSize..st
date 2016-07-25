converting-arrays
asByteArrayOfSize: aSize 
	"Answer a ByteArray of aSize with my value, most-significant byte first."
	| answer digitPos |
	aSize < self digitLength ifTrue: [ self error: 'number to large for byte array' ].
	answer := ByteArray new: aSize.
	digitPos := 1.
	aSize 
		to: aSize - self digitLength + 1
		by: -1
		do: 
			[ :pos | 
			answer 
				at: pos
				put: (self digitAt: digitPos).
			digitPos := digitPos + 1 ].
	^ answer