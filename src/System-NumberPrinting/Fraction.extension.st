Extension { #name : 'Fraction' }

{ #category : '*System-NumberPrinting' }
Fraction >> printOn: aStream base: base [

	aStream nextPut: $(.
	numerator printOn: aStream base: base.
	aStream nextPut: $/.
	denominator printOn: aStream base: base.
	aStream nextPut: $)
]

{ #category : '*System-NumberPrinting' }
Fraction >> storeOn: aStream base: base [

	aStream nextPut: $(.
	numerator storeOn: aStream base: base.
	aStream nextPut: $/.
	denominator storeOn: aStream base: base.
	aStream nextPut: $)
]
