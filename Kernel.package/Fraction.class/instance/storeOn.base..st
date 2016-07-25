printing
storeOn: aStream base: base

	aStream nextPut: $(.
	numerator storeOn: aStream base: base.
	aStream nextPut: $/.
	denominator storeOn: aStream base: base.
	aStream nextPut: $).
