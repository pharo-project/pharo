Extension { #name : #ScaledDecimal }

{ #category : #'*AST-Core-Parser' }
ScaledDecimal class >> readFrom: stringOrStream [ 
	"Answer a decimal number as described on stringOrStream.
	The number may not include a leading radix specification, as in 16rFADE,
	nor an exponent like 1.0e-3
	It might have a scale specification at end or not like 10.3s2
	If not, number of digits after decimal point will be used as scale"
	
	^(NumberParser on: stringOrStream) nextScaledDecimal
]
