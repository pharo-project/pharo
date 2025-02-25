Extension { #name : 'Fraction' }

{ #category : '*System-NumberPrinting' }
Fraction >> storeOn: aStream base: base [

	aStream nextPut: $(.
	numerator storeOn: aStream base: base.
	aStream nextPut: $/.
	denominator storeOn: aStream base: base.
	aStream nextPut: $)
]
