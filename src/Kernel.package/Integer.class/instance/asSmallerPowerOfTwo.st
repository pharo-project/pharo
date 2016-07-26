truncation and round off
asSmallerPowerOfTwo
	"Convert the receiver into a power of two which is not larger than the receiver"
	^self isPowerOfTwo
		ifTrue:[self]
		ifFalse:[self > 0 ifTrue: [	1 bitShift: (self highBit - 1)]
						ifFalse: [DomainError signal: 'Value outside (0 , infinity)' from: 0]]