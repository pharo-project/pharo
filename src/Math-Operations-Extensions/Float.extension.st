Extension { #name : #Float }

{ #category : #'*Math-Operations-Extensions' }
Float >> arcCos [
	"Answer the angle in radians."

	^ Halfpi - self arcSin
]

{ #category : #'*Math-Operations-Extensions' }
Float >> arcSin [
	"Answer the angle in radians."

	((self < -1.0) or: [self > 1.0]) ifTrue: [^DomainError signal: 'Value out of range' from: -1 to: 1].
	((self = -1.0) or: [self = 1.0])
		ifTrue: [^ Halfpi * self]
		ifFalse: [^ (self / (1.0 - (self * self)) sqrt) arcTan]
]

{ #category : #'*Math-Operations-Extensions' }
Float >> arcTan: denominator [
	"Answer the angle in radians.
	Implementation note: use sign in order to catch cases of negativeZero"
	
	"(-0.5 arcTan: -0.5) >>> -2.356194490192345"
	"(1.2 arcTan: 1.5) >>>  0.6747409422235526"
	"(1.2 arcTan: -1.5) >>> 2.4668517113662407"
	^ self = 0.0
		ifTrue: [denominator signBit = 0
			ifTrue: [ 0 ]
			ifFalse: [ self signBit = 0
				ifTrue: [ Pi ]
				ifFalse: [ Pi negated ]]]
		ifFalse: [denominator = 0.0
			ifTrue: [self > 0.0
				ifTrue: [ Halfpi ]
				ifFalse: [ Halfpi negated ]]
			ifFalse: [denominator > 0
				ifTrue: [ (self / denominator) arcTan ]
				ifFalse: [self > 0
					ifTrue: [ ((self / denominator) arcTan) + Pi ]
					ifFalse: [ ((self / denominator) arcTan) - Pi ]]]]
]

{ #category : #'*Math-Operations-Extensions' }
Float >> asIEEE32BitWord [
	"Convert the receiver into a 32 bit Integer value representing the same number in IEEE 32 bit format.
	Used for conversion in FloatArrays only."
	
	| word1 word2 sign mantissa exponent destWord truncatedBits mask roundToUpper |
	
	"skip fast positive and nnegative zero"
	self = 0.0 ifTrue: [^self basicAt: 1].
	
	"retrieve 64 bits of IEEE 754 double"
	word1 := self basicAt: 1.
	word2 := self basicAt: 2.
	
	"prepare sign exponent and mantissa of 32 bits float"
	sign := word1 bitAnd: 16r80000000.
	exponent := ((word1 bitShift: -20) bitAnd: 16r7FF) - 1023 + 127.
	mantissa := (word2 bitShift: -29) + ((word1 bitAnd:  16rFFFFF) bitShift: 3).
	truncatedBits := (word2 bitAnd: 16r1FFFFFFF).

	"We must now honour default IEEE rounding mode (round to nearest even)"
	
	"we are below gradual underflow, even if rounded to upper mantissa"
	exponent < -24 ifTrue: [^sign "this can be negative zero"].
	
	"BEWARE: rounding occurs on less than 23bits when gradual underflow"
	exponent <= 0
		ifTrue:
			[mask := 1 bitShift: exponent negated.
			mantissa := mantissa bitOr: 16r800000.
			roundToUpper := (mantissa bitAnd: mask) isZero not
				and: [truncatedBits isZero not
					or: [(mantissa bitAnd: mask - 1) isZero not
						or: [(mantissa bitAnd: mask*2) isZero not]]].
			mantissa := mantissa bitShift: exponent - 1.
			"exponent := exponent + 1"]
		ifFalse:
			[roundToUpper := (truncatedBits bitAnd: 16r10000000) isZero not
				and: [(mantissa bitAnd: 16r1) isZero not
					or: [(truncatedBits bitAnd: 16r0FFFFFFF) isZero not]]
			].
		
	"adjust mantissa and exponent due to IEEE rounding mode"
	roundToUpper
		ifTrue:
			[mantissa := mantissa + 1.
			mantissa > 16r7FFFFF
				ifTrue:
					[mantissa := 0.
					exponent := exponent+1]].

	exponent > 254 ifTrue: ["Overflow"
		exponent := 255.
		self isNaN
			ifTrue: [mantissa isZero
				ifTrue: ["BEWARE: do not convert a NaN to infinity due to truncatedBits"
					mantissa := 1]]
			ifFalse: [mantissa := 0]].
		
	"Encode the word"
	destWord := (sign bitOr: ((exponent max: 0) bitShift: 23)) bitOr: mantissa.
	^ destWord
]

{ #category : #'*Math-Operations-Extensions' }
Float >> closeTo: num [
 	"Tell whether the receiver and arguments are close from each."
	
	^ self closeTo: num precision: 0.0001
]

{ #category : #'*Math-Operations-Extensions' }
Float >> closeTo: num precision: aPrecision [
 	"Tell whether the receiver and arguments are close from each other given a precision"
	
	num isNumber ifFalse: [ ^ [self = num] ifError: [false]].
	self = 0.0 ifTrue: [^ num abs < aPrecision].
	num = 0 ifTrue: [^self abs < aPrecision].
	^self = num asFloat
		or: [(self - num) abs / (self abs max: num abs) < aPrecision]
]

{ #category : #'*Math-Operations-Extensions' }
Float >> cos [
	"Answer the cosine of the receiver taken as an angle in radians."

	^ (self + Halfpi) sin
]

{ #category : #'*Math-Operations-Extensions' }
Float >> degreeCos [
	"Take care of exceptional values"
	
	self isFinite ifTrue: [^super degreeCos].
	^self degreesToRadians cos
]

{ #category : #'*Math-Operations-Extensions' }
Float >> degreeSin [
	"Take care of exceptional values"
	
	self isFinite ifTrue: [^super degreeSin].
	^self degreesToRadians sin
]

{ #category : #'*Math-Operations-Extensions' }
Float >> degreesToRadians [
	"Answer the receiver in radians. Assumes the receiver is in degrees."

	^self * RadiansPerDegree
]

{ #category : #'*Math-Operations-Extensions' }
Float class >> epsilon [
	"Answer difference between 1.0 and previous representable value"
	
	^1.0 timesTwoPower: 1 - self precision
]

{ #category : #'*Math-Operations-Extensions' }
Float class >> fmax [
	"Answer the maximum finite floating point value representable."
	
	^MaxVal
]

{ #category : #'*Math-Operations-Extensions' }
Float class >> fmin [
	"Answer minimum positive representable value."
	
	^self denormalized
		ifTrue: [self fminDenormalized]
		ifFalse: [self fminNormalized]
]

{ #category : #'*Math-Operations-Extensions' }
Float class >> fminDenormalized [
	"Answer the minimum denormalized value representable."
	
	^1.0 timesTwoPower: MinValLogBase2
]

{ #category : #'*Math-Operations-Extensions' }
Float class >> fminNormalized [
	"Answer the minimum normalized value representable."
	
	^1.0 timesTwoPower: -1022
]

{ #category : #'*Math-Operations-Extensions' }
Float >> integerPart [
	"Answer a Float whose value is the receiver's truncated value."

	^self - self fractionPart
]

{ #category : #'*Math-Operations-Extensions' }
Float >> log [
	"Answer the base 10 logarithm of the receiver."

	^ self ln / Ln10
]

{ #category : #'*Math-Operations-Extensions' }
Float >> nthRoot: aPositiveInteger [
	"Answer the nth root of the receiver."
	aPositiveInteger = 2 ifTrue: [
		^self sqrt ].

	(aPositiveInteger isInteger not or: [ aPositiveInteger negative ])
		ifTrue: [^ ArithmeticError signal: 'nth root only defined for positive Integer n.'].
	
	^self negative
		ifTrue: [
			aPositiveInteger odd
				ifTrue: [ (self negated raisedTo: 1.0 / aPositiveInteger) negated ]
				ifFalse: [ ArithmeticError signal: 'Negative numbers don''t have even roots.' ]]
		ifFalse: [ self raisedTo: 1.0 / aPositiveInteger ]
]

{ #category : #'*Math-Operations-Extensions' }
Float >> predecessor [
	| ulp |
	self isFinite ifFalse: [
		(self isNaN or: [self negative]) ifTrue: [^self].
		^Float fmax].
	self = 0.0 ifTrue: [^Float fmin negated].
	ulp := self ulp.
	^self - (0.5 * ulp) = self
		ifTrue: [self - ulp]
		ifFalse: [self - (0.5 * ulp)]
]

{ #category : #'*Math-Operations-Extensions' }
Float >> radiansToDegrees [
	"Answer the receiver in degrees. Assumes the receiver is in radians."

	^self / RadiansPerDegree
]

{ #category : #'*Math-Operations-Extensions' }
Float >> reciprocal [

	"Returns the reciprocal.
	If self is 0.0 the / signals a ZeroDivide"
	
	^1.0 / self
]

{ #category : #'*Math-Operations-Extensions' }
Float >> reduce [
    "If self is close to an integer, return that integer"

    (self closeTo: self rounded) ifTrue: [^ self rounded]
]

{ #category : #'*Math-Operations-Extensions' }
Float >> round: numberOfWishedDecimal [
	"Round the decimal part of the receiver to be limited to the number of wished decimal. Only leave a fixed amount of decimal"
   "(10.12345 round: 2) >>> 10.12 "
        
	^(self asFraction round: numberOfWishedDecimal) asFloat

]

{ #category : #'*Math-Operations-Extensions' }
Float >> rounded [
	"Answer the integer nearest the receiver.
	Implementation note: super would not handle tricky inexact arithmetic"
	
	"self assert: 5000000000000001.0 rounded = 5000000000000001"

	self fractionPart abs < 0.5
		ifTrue: [^self truncated]
		ifFalse: [^self truncated + self sign rounded]
]

{ #category : #'*Math-Operations-Extensions' }
Float >> safeArcCos [
	"Answer the angle in radians."
	(self between: -1.0 and: 1.0)
		ifTrue: [^ self arcCos]
		ifFalse: [^ self sign arcCos]
]

{ #category : #'*Math-Operations-Extensions' }
Float >> successor [
	| ulp |
	self isFinite ifFalse: [
		(self isNaN or: [self positive]) ifTrue: [^self].
		^Float fmax negated].
	self = 0.0 ifTrue: [^Float fmin].
	ulp := self ulp.
	^self + (0.5 * ulp) = self
		ifTrue: [self + ulp]
		ifFalse: [self + (0.5 * ulp)]
]

{ #category : #'*Math-Operations-Extensions' }
Float >> tan [
	"Answer the tangent of the receiver taken as an angle in radians."

	^ self sin / self cos
]

{ #category : #'*Math-Operations-Extensions' }
Float >> ulp [
	"Answer the unit of least precision of self (the power of two corresponding to last bit of mantissa)"
	
	| exponent |
	self isFinite ifFalse: [
		self isNaN ifTrue: [^self].
		^Float infinity].
	self = 0.0 ifTrue: [^Float fmin].
	exponent := self exponent.
	^exponent < self class emin
		ifTrue: [Float fminDenormalized]
 		ifFalse: [Float epsilon timesTwoPower: exponent]
]
