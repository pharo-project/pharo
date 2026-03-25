Extension { #name : 'Number' }

{ #category : '*Math-Operations-Extensions' }
Number >> % aNumber [
	"modulo. Remainder defined in terms of //. Answer a Number with the
	same sign as aNumber."

	"9 % 4 >>> 1"
	"-9 % 4 >>> 3"
	"9 % -4 >>> -3"


	^ self \\ aNumber
]

{ #category : '*Math-Operations-Extensions' }
Number >> ** exponent [
	" A shortcut method for raisedTo: "

	"2 ** 8 >>> 256"
	"8 ** 2 >>> 64"
	"2 ** (1/12) >>> 1.0594630943592953"
	"2 ** -1 >>> (1/2)"

	^ self raisedTo: exponent
]

{ #category : '*Math-Operations-Extensions' }
Number >> adaptToCollection: rcvr andSend: selector [
	"If I am involved in arithmetic with a Collection, return a Collection of
	the results of each element combined with me in that expression."

	^ rcvr collect: [:element | element perform: selector with: self]
]

{ #category : '*Math-Operations-Extensions' }
Number >> adaptToPoint: rcvr andSend: selector [
	"If I am involved in arithmetic with a Point, convert me to a Point."
	^ rcvr perform: selector with: self@self
]

{ #category : '*Math-Operations-Extensions' }
Number >> arcCos [
	"The receiver is the cosine of an angle. Answer the angle measured in
	radians."

	^self asFloat arcCos
]

{ #category : '*Math-Operations-Extensions' }
Number >> arcSin [
	"The receiver is the sine of an angle. Answer the angle measured in
	radians."

	^self asFloat arcSin
]

{ #category : '*Math-Operations-Extensions' }
Number >> arcTan [
	"The receiver is the tangent of an angle. Answer the angle measured in
	radians."

	^self asFloat arcTan
]

{ #category : '*Math-Operations-Extensions' }
Number >> arcTan: denominator [
	"The receiver is the tangent of an angle. Answer the angle measured in
	radians."

	^ self asFloat arcTan: denominator
]

{ #category : '*Math-Operations-Extensions' }
Number >> closeTo: num [
	"Tell whether the receiver and arguments are close from each."

	num isFloat ifTrue: [ ^ num closeTo: self asFloat ].
	^ [ self = num ] onErrorDo: [ false ]
]

{ #category : '*Math-Operations-Extensions' }
Number >> closeTo: num precision: aPrecision [
	"are these two numbers close?"

	num isFloat ifTrue: [ ^ num closeTo: self asFloat precision: aPrecision ].
	^ [ self = num ] onErrorDo: [ false ]
]

{ #category : '*Math-Operations-Extensions' }
Number >> cos [
	"The receiver represents an angle measured in radians. Answer its cosine."

	^self asFloat cos
]

{ #category : '*Math-Operations-Extensions' }
Number >> degreeCos [
	"Answer the cosine of the receiver taken as an angle in degrees."

	^ (90 - (180 + self \\ 360 - 180) abs) degreesToRadians sin
]

{ #category : '*Math-Operations-Extensions' }
Number >> degreeSin [
	"Answer the sine of the receiver taken as an angle in degrees."

	^(90 - self) degreeCos
]

{ #category : '*Math-Operations-Extensions' }
Number >> degreesToRadians [
	"The receiver is assumed to represent degrees. Answer the conversion to
	radians."

	^self asFloat degreesToRadians
]

{ #category : '*Math-Operations-Extensions' }
Number >> even [
	"Answer whether the receiver is an even number."

	^self \\ 2 = 0
]

{ #category : '*Math-Operations-Extensions' }
Number >> floorLog: radix [
	"Answer the floor of the log base radix of the receiver."

	^(self log: radix) floor
]

{ #category : '*Math-Operations-Extensions' }
Number >> fractionPart [
	"Added for ANSI compatibility"
	^self - self integerPart
]

{ #category : '*Math-Operations-Extensions' }
Number >> integerPart [
	"Added for ANSI compatibility"
	^self truncated
]

{ #category : '*Math-Operations-Extensions' }
Number >> interpolateTo: aNumber at: param [
    ^  self * (1 - param) + (aNumber * param)
]

{ #category : '*Math-Operations-Extensions' }
Number >> isDivisibleBy: aNumber [
	aNumber = 0 ifTrue: [^ false].
	aNumber isInteger ifFalse: [^ false].
	^ (self \\ aNumber) = 0
]

{ #category : '*Math-Operations-Extensions' }
Number >> ln [
	"Answer the natural log of the receiver."

	^self asFloat ln
]

{ #category : '*Math-Operations-Extensions' }
Number >> log [
	"Answer the base-10 log of the receiver."

	^self asFloat log
]

{ #category : '*Math-Operations-Extensions' }
Number >> log: aNumber [
	"Answer the log base aNumber of the receiver."

	^self ln / aNumber ln
]

{ #category : '*Math-Operations-Extensions' }
Number >> nthRoot: aPositiveInteger [
	"Answer the nth root of the receiver."

	self subclassResponsibility
]

{ #category : '*Math-Operations-Extensions' }
Number >> odd [
	"Answer whether the receiver is an odd number."

	^self even == false
]

{ #category : '*Math-Operations-Extensions' }
Number >> percent [

	^ self / 100
]

{ #category : '*Math-Operations-Extensions' }
Number >> radiansToDegrees [
	"The receiver is assumed to represent radians. Answer the conversion to
	degrees."

	"Float pi radiansToDegrees >>> 180.0"

	^self asFloat radiansToDegrees
]

{ #category : '*Math-Operations-Extensions' }
Number >> raisedToFraction: aFraction [
	self isZero
		ifTrue:
			[aFraction negative ifTrue: [^ (ZeroDivide dividend: 1) signal].
			^self].
	self negative ifFalse: [^(self ln * aFraction) exp].
	aFraction denominator even
		ifTrue: [^ ArithmeticError signal: 'nth root only defined for positive Integer n.'].
	^(self negated ln * aFraction) exp negated
]

{ #category : '*Math-Operations-Extensions' }
Number >> raisedToInteger: anInteger [

	"The 0 raisedToInteger: 0 is an special case. In some contexts must be 1 and in others must
	be handled as an indeterminate form.
	I take the first context because that's the way that was previously handled.
	Maybe further discussion is required on this topic."

	|bitProbe result|

	anInteger negative ifTrue: [^(self raisedToInteger: anInteger negated) reciprocal].
	bitProbe := 1 bitShift: anInteger highBit - 1.
 	result := self class one.
  	[
		(anInteger bitAnd: bitProbe) = 0 ifFalse: [result := result * self].
       bitProbe := bitProbe bitShift: -1.
		bitProbe > 0 ]
	whileTrue: [result := result * result].

	^result
]

{ #category : '*Math-Operations-Extensions' }
Number >> reciprocal [
	"Returns the reciprocal of self.
	In case self is 0 the / signals ZeroDivide"

	"1/2 reciprocal >>> 2"
	"2 reciprocal >>> (1/2)"
	"1.25 reciprocal >>> 0.8"
	"-2 reciprocal >>> (-1/2)"

	^1 / self
]

{ #category : '*Math-Operations-Extensions' }
Number >> reduce [
    "If self is close to an integer, return that integer"
    ^ self
]

{ #category : '*Math-Operations-Extensions' }
Number >> round: numberOfWishedDecimal [
	"Round the decimal part of the receiver to be limited to the number of wished decimal. Only leave a fixed amount of decimal"
   "(10.12345 round: 2) >>> 10.12"
	"(10.199999999 round: 2) >>>10.2"

	^ self subclassResponsibility
]

{ #category : '*Math-Operations-Extensions' }
Number >> rounded [
	"Answer the integer nearest the receiver."

	"1.4 rounded >>> 1"
	"1.5 rounded >>> 2"
	"2 rounded >>> 2"
	"-1.5 rounded >>> -2"

	^(self + (self sign / 2)) truncated
]

{ #category : '*Math-Operations-Extensions' }
Number >> sin [
	"The receiver represents an angle measured in radians. Answer its sine."

	^self asFloat sin
]

{ #category : '*Math-Operations-Extensions' }
Number >> sqrt [
	"Answer the square root of the receiver."

	self subclassResponsibility
]

{ #category : '*Math-Operations-Extensions' }
Number >> squared [
	"Answer the receiver multiplied by itself."

	^self * self
]

{ #category : '*Math-Operations-Extensions' }
Number >> tan [
	"The receiver represents an angle measured in radians. Answer its
	tangent."

	^self asFloat tan
]
