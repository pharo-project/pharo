truncation and round off
asLargerPowerOfTwo
	"Convert the receiver into a power of two which is not less than the receiver"
	| quotient |
	(numerator = 0 or: [numerator sign ~= denominator sign]) ifTrue: [^DomainError signal: 'Value outside (0 , infinity)' from: 0].
	^(quotient := denominator // numerator) > 0 
		ifTrue: [Fraction numerator: 1 denominator:  (1 bitShift: (quotient highBit -1))]
		ifFalse: [quotient := numerator // denominator.
				"If my quotient is a power of two, we, we need to check remainder, to see if we should shift by highbit or not.
				 (This is equivalent to Integer asLargerPowerOfTwo returning self when receiver is power of two) "
				(quotient isPowerOfTwo and: [numerator \\ denominator = 0])
					ifTrue: [quotient]
					ifFalse: [1 bitShift: (quotient highBit )]]