truncation and round off
asSmallerPowerOfTwo
	"Convert the receiver into a power of two which is not larger than the receiver"
	| quotient |
	(numerator = 0 or: [numerator sign ~= denominator sign]) ifTrue: [^DomainError signal: 'Value outside (0 , infinity)' from: 0].
	^(quotient := denominator // numerator) > 0 
		ifTrue: [	"If my quotient is a power of two, we, we need to check remainder, to see if we should shift by highbit or not.
				 (This is equivalent to Integer asSmallerPowerOfTwo returning self when receiver is power of two) "
				 (quotient isPowerOfTwo and: [denominator \\ numerator = 0]) 
					ifTrue: [Fraction numerator: 1 denominator: quotient]
					ifFalse:[Fraction numerator: 1 denominator:  (1 bitShift: quotient highBit)]]
					
		ifFalse: [1 bitShift: ((numerator // denominator) highBit -1)]