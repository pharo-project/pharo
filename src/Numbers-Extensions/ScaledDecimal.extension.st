Extension { #name : #ScaledDecimal }

{ #category : #'*Numbers-Extensions' }
ScaledDecimal >> nthRoot: anInteger [
	"Answer the nth root of the receiver.
	Preserve receiver class and scale if answer is exact.
	Otherwise, answer a Float to denote inexactness."
	| nthRoot |
	nthRoot := self asFraction nthRoot: anInteger.
	^nthRoot isFloat
		ifTrue: [nthRoot]
		ifFalse: [nthRoot asScaledDecimal: scale]
]

{ #category : #'*Numbers-Extensions' }
ScaledDecimal >> round: numberOfWishedDecimal [
	"Round the decimal part of the receiver to be limited to the number of wished decimal. Only leave a fixed amount of decimal."
	
   "10.156s round: 2 >>> 10.160s3 "
   "1/3.0s round: 4 >>> 0.3333s4 "

	^(super round: numberOfWishedDecimal) asScaledDecimal: (scale max: numberOfWishedDecimal)
]

{ #category : #'*Numbers-Extensions' }
ScaledDecimal >> sqrt [
	"Answer the square root of the receiver.
	Preserve receiver class and scale if answer is exact.
	Otherwise, answer a Float to denote inexactness."
	| squareRoot |
	squareRoot := self asFraction sqrt.
	^squareRoot isFloat
		ifTrue: [squareRoot]
		ifFalse: [squareRoot asScaledDecimal: scale]
]
