Extension { #name : #Number }

{ #category : #'*Numbers-Extensions' }
Number >> % aNumber [ 
	"modulo. Remainder defined in terms of //. Answer a Number with the 
	same sign as aNumber. e.g. 9\\4 = 1, -9\\4 = 3, 9\\-4 = -3, 0.9\\0.4 = 0.1."
	^ self \\ aNumber
]

{ #category : #'*Numbers-Extensions' }
Number >> ** exponent [ 
	" A shortcut methog for raisedTo: "
	^ self raisedTo: exponent
]

{ #category : #'*Numbers-Extensions' }
Number >> adaptToCollection: rcvr andSend: selector [
	"If I am involved in arithmetic with a Collection, return a Collection of
	the results of each element combined with me in that expression."

	^ rcvr collect: [:element | element perform: selector with: self]
]

{ #category : #'*Numbers-Extensions' }
Number >> adaptToPoint: rcvr andSend: selector [
	"If I am involved in arithmetic with a Point, convert me to a Point."
	^ rcvr perform: selector with: self@self
]

{ #category : #'*Numbers-Extensions' }
Number >> arcCos [ 
	"The receiver is the cosine of an angle. Answer the angle measured in 
	radians."

	^self asFloat arcCos
]

{ #category : #'*Numbers-Extensions' }
Number >> arcSin [
	"The receiver is the sine of an angle. Answer the angle measured in 
	radians."

	^self asFloat arcSin
]

{ #category : #'*Numbers-Extensions' }
Number >> arcTan [
	"The receiver is the tangent of an angle. Answer the angle measured in 
	radians."

	^self asFloat arcTan
]

{ #category : #'*Numbers-Extensions' }
Number >> arcTan: denominator [
	"The receiver is the tangent of an angle. Answer the angle measured in 
	radians."

	^(self asFloat) arcTan: denominator.
]

{ #category : #'*Numbers-Extensions' }
Number >> closeTo: num [
	"Tell whether the receiver and arguments are close from each."

	num isFloat ifTrue: [ ^ num closeTo: self asFloat ].
	^ [ self = num ] ifError: [ false ]
]

{ #category : #'*Numbers-Extensions' }
Number >> closeTo: num precision: aPrecision [
	"are these two numbers close?"

	num isFloat ifTrue: [ ^ num closeTo: self asFloat precision: aPrecision ].
	^ [ self = num ] ifError: [ false ]
]

{ #category : #'*Numbers-Extensions' }
Number >> cos [
	"The receiver represents an angle measured in radians. Answer its cosine."

	^self asFloat cos
]

{ #category : #'*Numbers-Extensions' }
Number >> degreeCos [
	"Answer the cosine of the receiver taken as an angle in degrees."
	
	^ (90 - (180 + self \\ 360 - 180) abs) degreesToRadians sin
]

{ #category : #'*Numbers-Extensions' }
Number >> degreeSin [
	"Answer the sine of the receiver taken as an angle in degrees."
	
	^(90 - self) degreeCos
]

{ #category : #'*Numbers-Extensions' }
Number >> degreesToRadians [
	"The receiver is assumed to represent degrees. Answer the conversion to 
	radians."

	^self asFloat degreesToRadians
]

{ #category : #'*Numbers-Extensions' }
Number >> even [
	"Answer whether the receiver is an even number."

	^self \\ 2 = 0
]

{ #category : #'*Numbers-Extensions' }
Number >> floorLog: radix [
	"Answer the floor of the log base radix of the receiver."

	^(self log: radix) floor
]

{ #category : #'*Numbers-Extensions' }
Number >> interpolateTo: aNumber at: param [
    ^  self * (1 - param) + (aNumber * param)
]

{ #category : #'*Numbers-Extensions' }
Number >> isDivisibleBy: aNumber [
	aNumber = 0 ifTrue: [^ false].
	aNumber isInteger ifFalse: [^ false].
	^ (self \\ aNumber) = 0
]

{ #category : #'*Numbers-Extensions' }
Number >> ln [
	"Answer the natural log of the receiver."

	^self asFloat ln
]

{ #category : #'*Numbers-Extensions' }
Number >> log [
	"Answer the base-10 log of the receiver."

	^self asFloat log
]

{ #category : #'*Numbers-Extensions' }
Number >> log: aNumber [ 
	"Answer the log base aNumber of the receiver."

	^self ln / aNumber ln
]

{ #category : #'*Numbers-Extensions' }
Number >> nthRoot: aPositiveInteger [
	"Answer the nth root of the receiver."

	self subclassResponsibility
]

{ #category : #'*Numbers-Extensions' }
Number >> odd [
	"Answer whether the receiver is an odd number."

	^self even == false
]

{ #category : #'*Numbers-Extensions' }
Number >> percent [

	^ self / 100
]

{ #category : #'*Numbers-Extensions' }
Number >> radiansToDegrees [
	"The receiver is assumed to represent radians. Answer the conversion to 
	degrees."

	^self asFloat radiansToDegrees
]

{ #category : #'*Numbers-Extensions' }
Number >> sin [
	"The receiver represents an angle measured in radians. Answer its sine."

	^self asFloat sin
]

{ #category : #'*Numbers-Extensions' }
Number >> sqrt [
	"Answer the square root of the receiver."

	self subclassResponsibility
]

{ #category : #'*Numbers-Extensions' }
Number >> squared [
	"Answer the receiver multiplied by itself."

	^self * self
]

{ #category : #'*Numbers-Extensions' }
Number >> strictlyPositive [
	"Answer whether the receiver is mathematically positive."

	^ self > 0
]

{ #category : #'*Numbers-Extensions' }
Number >> tan [
	"The receiver represents an angle measured in radians. Answer its 
	tangent."

	^self asFloat tan
]
