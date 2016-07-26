printing
printOn: aStream base: base

	aStream nextPut: $(.
	numerator printOn: aStream base: base.
	aStream nextPut: $/.
	denominator printOn: aStream base: base.
	aStream nextPut: $).
