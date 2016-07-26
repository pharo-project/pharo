printing
printOn: aStream base: base
	base = 10 ifFalse: [self error: 'ScaledDecimals should be printed only in base 10'].
	^self printOn: aStream