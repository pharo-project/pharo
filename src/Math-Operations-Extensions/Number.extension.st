Extension { #name : #Number }

{ #category : #'*Math-Operations-Extensions' }
Number >> % aNumber [ 
	"modulo. Remainder defined in terms of //. Answer a Number with the 
	same sign as aNumber."
	
	"9 % 4 >>> 1"
	"-9 % 4 >>> 3"
	"9 % -4 >>> -3"
	
	
	^ self \\ aNumber
]

{ #category : #'*Math-Operations-Extensions' }
Number >> ** exponent [ 
	" A shortcut method for raisedTo: "
	
	"2 ** 8 >>> 256"
	"8 ** 2 >>> 64"
	"2 ** (1/12) >>> 1.0594630943592953"
	"2 ** -1 >>> (1/2)"
	
	^ self raisedTo: exponent
]

{ #category : #'*Math-Operations-Extensions' }
Number >> adaptToCollection: rcvr andSend: selector [
	"If I am involved in arithmetic with a Collection, return a Collection of
	the results of each element combined with me in that expression."

	^ rcvr collect: [:element | element perform: selector with: self]
]

{ #category : #'*Math-Operations-Extensions' }
Number >> adaptToPoint: rcvr andSend: selector [
	"If I am involved in arithmetic with a Point, convert me to a Point."
	^ rcvr perform: selector with: self@self
]

{ #category : #'*Math-Operations-Extensions' }
Number >> arcCos [ 
	"The receiver is the cosine of an angle. Answer the angle measured in 
	radians."

	^self asFloat arcCos
]

{ #category : #'*Math-Operations-Extensions' }
Number >> arcSin [
	"The receiver is the sine of an angle. Answer the angle measured in 
	radians."

	^self asFloat arcSin
]

{ #category : #'*Math-Operations-Extensions' }
Number >> arcTan [
	"The receiver is the tangent of an angle. Answer the angle measured in 
	radians."

	^self asFloat arcTan
]

{ #category : #'*Math-Operations-Extensions' }
Number >> arcTan: denominator [
	"The receiver is the tangent of an angle. Answer the angle measured in 
	radians."

	^ self asFloat arcTan: denominator
]

{ #category : #'*Math-Operations-Extensions' }
Number >> closeTo: num [
	"Tell whether the receiver and arguments are close from each."

	num isFloat ifTrue: [ ^ num closeTo: self asFloat ].
	^ [ self = num ] ifError: [ false ]
]

{ #category : #'*Math-Operations-Extensions' }
Number >> closeTo: num precision: aPrecision [
	"are these two numbers close?"

	num isFloat ifTrue: [ ^ num closeTo: self asFloat precision: aPrecision ].
	^ [ self = num ] ifError: [ false ]
]

{ #category : #'*Math-Operations-Extensions' }
Number >> cos [
	"The receiver represents an angle measured in radians. Answer its cosine."

	^self asFloat cos
]

{ #category : #'*Math-Operations-Extensions' }
Number >> degreeCos [
	"Answer the cosine of the receiver taken as an angle in degrees."
	
	^ (90 - (180 + self \\ 360 - 180) abs) degreesToRadians sin
]

{ #category : #'*Math-Operations-Extensions' }
Number >> degreeSin [
	"Answer the sine of the receiver taken as an angle in degrees."
	
	^(90 - self) degreeCos
]

{ #category : #'*Math-Operations-Extensions' }
Number >> degreesToRadians [
	"The receiver is assumed to represent degrees. Answer the conversion to 
	radians."

	^self asFloat degreesToRadians
]

{ #category : #'*Math-Operations-Extensions' }
Number >> even [
	"Answer whether the receiver is an even number."

	^self \\ 2 = 0
]

{ #category : #'*Math-Operations-Extensions' }
Number >> floorLog: radix [
	"Answer the floor of the log base radix of the receiver."

	^(self log: radix) floor
]

{ #category : #'*Math-Operations-Extensions' }
Number >> interpolateTo: aNumber at: param [
    ^  self * (1 - param) + (aNumber * param)
]

{ #category : #'*Math-Operations-Extensions' }
Number >> isDivisibleBy: aNumber [
	aNumber = 0 ifTrue: [^ false].
	aNumber isInteger ifFalse: [^ false].
	^ (self \\ aNumber) = 0
]

{ #category : #'*Math-Operations-Extensions' }
Number >> ln [
	"Answer the natural log of the receiver."

	^self asFloat ln
]

{ #category : #'*Math-Operations-Extensions' }
Number >> log [
	"Answer the base-10 log of the receiver."

	^self asFloat log
]

{ #category : #'*Math-Operations-Extensions' }
Number >> log: aNumber [ 
	"Answer the log base aNumber of the receiver."

	^self ln / aNumber ln
]

{ #category : #'*Math-Operations-Extensions' }
Number >> nthRoot: aPositiveInteger [
	"Answer the nth root of the receiver."

	self subclassResponsibility
]

{ #category : #'*Math-Operations-Extensions' }
Number >> odd [
	"Answer whether the receiver is an odd number."

	^self even == false
]

{ #category : #'*Math-Operations-Extensions' }
Number >> percent [

	^ self / 100
]

{ #category : #'*Math-Operations-Extensions' }
Number >> radiansToDegrees [
	"The receiver is assumed to represent radians. Answer the conversion to 
	degrees."
	
	"Float pi radiansToDegrees >>> 180.0"

	^self asFloat radiansToDegrees
]

{ #category : #'*Math-Operations-Extensions' }
Number >> sin [
	"The receiver represents an angle measured in radians. Answer its sine."

	^self asFloat sin
]

{ #category : #'*Math-Operations-Extensions' }
Number >> sqrt [
	"Answer the square root of the receiver."

	self subclassResponsibility
]

{ #category : #'*Math-Operations-Extensions' }
Number >> squared [
	"Answer the receiver multiplied by itself."

	^self * self
]

{ #category : #'*Math-Operations-Extensions' }
Number >> strictlyPositive [
	"Answer whether the receiver is mathematically positive."

	^ self > 0
]

{ #category : #'*Math-Operations-Extensions' }
Number >> tan [
	"The receiver represents an angle measured in radians. Answer its 
	tangent."

	^self asFloat tan
]
