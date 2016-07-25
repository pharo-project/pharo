testing
isPowerOfTwo
	|reduced|
	reduced := self reduced.
	^(reduced numerator = 1 and: [reduced denominator isPowerOfTwo])
	 or: [reduced denominator = 1 and: [reduced numerator isPowerOfTwo]]