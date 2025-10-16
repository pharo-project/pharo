Extension { #name : 'ScaledDecimal' }

{ #category : '*System-NumberPrinting' }
ScaledDecimal >> printOn: aStream base: base [
	base = 10 ifFalse: [self error: 'ScaledDecimals should be printed only in base 10'].
	self printOn: aStream
]

{ #category : '*System-NumberPrinting' }
ScaledDecimal >> storeOn: aStream [
	"ScaledDecimal sometimes have more digits than they print (potentially an infinity).
	In this case, do not use printOn: because it would loose some extra digits"

	self shouldBePrintedAsLiteral
		ifTrue: [self printOn: aStream]
		ifFalse: [aStream
			nextPut: $(;
		 	store: numerator;
			nextPut: $/;
			store: denominator;
			nextPut: $s;
			store: scale;
			nextPut: $)]
]
