printing
printOn: aStream showingDecimalPlaces: placesDesired
	"Same as super, but provides a faster implementation by inlining some Fraction protocol thus avoiding intermediate Fraction creation."
	
	| roundedFractionPart integerPart scaling |
	placesDesired <= 0
		ifTrue: [self rounded printOn: aStream]
		ifFalse:
			[scaling := 10 raisedToInteger: placesDesired.
			integerPart := numerator abs quo: denominator.
			roundedFractionPart := (numerator abs - (integerPart * denominator)) * scaling * 2 + denominator quo: denominator * 2.
			roundedFractionPart = scaling
				ifTrue:
					[integerPart := integerPart + 1.
					roundedFractionPart := 0].
			"Don't print minus sign if result is rouded to zero"
			(numerator negative and: [integerPart > 0 or: [roundedFractionPart > 0]]) ifTrue: [aStream nextPut: $-].
			integerPart printOn: aStream.
			aStream nextPut: $..
			roundedFractionPart printOn: aStream base: 10 length: placesDesired padded: true].