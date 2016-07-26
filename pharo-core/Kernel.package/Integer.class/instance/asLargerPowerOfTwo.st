truncation and round off
asLargerPowerOfTwo
	"Convert the receiver into a power of two which is not less than the receiver"
	^self isPowerOfTwo
		ifTrue:[self]
		ifFalse:[self > 0 ifTrue: [	1 bitShift: (self highBit)]
						ifFalse: [DomainError signal: 'Value outside (0 , infinity)' from: 0]]