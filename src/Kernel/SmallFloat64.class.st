"
My instances represent 64-bit Floats whose exponent fits in 8 bits as immediate objects.  This representation is only available on 64-bit systems, not 32-bit systems.
"
Class {
	#name : #SmallFloat64,
	#superclass : #Float,
	#type : #immediate,
	#category : #'Kernel-Numbers'
}

{ #category : #'instance creation' }
SmallFloat64 class >> basicNew [
	self error: 'SmallFloat64s can only be created by performing arithmetic'
]

{ #category : #'instance creation' }
SmallFloat64 class >> basicNew: anInteger [
	^self basicNew
]

{ #category : #arithmetic }
SmallFloat64 >> * aNumber [ 
	"Primitive. Answer the result of multiplying the receiver by aNumber.
	Fail if the argument is not a Float. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 549>
	^ aNumber adaptToFloat: self andSend: #*
]

{ #category : #arithmetic }
SmallFloat64 >> + aNumber [ 
	"Primitive. Answer the sum of the receiver and aNumber. Essential.
	Fail if the argument is not a Float. See Object documentation
	whatIsAPrimitive."

	<primitive: 541>
	^ aNumber adaptToFloat: self andSend: #+
]

{ #category : #arithmetic }
SmallFloat64 >> - aNumber [ 
	"Primitive. Answer the difference between the receiver and aNumber.
	Fail if the argument is not a Float. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 542>
	^ aNumber adaptToFloat: self andSend: #-
]

{ #category : #arithmetic }
SmallFloat64 >> / aNumber [ 
	"Primitive. Answer the result of dividing receiver by aNumber.
	Fail if the argument is not a Float. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 550>
	aNumber = 0.0 ifTrue: [ ^ ZeroDivide signalWithDividend: self].
	^aNumber adaptToFloat: self andSend: #/
]

{ #category : #comparing }
SmallFloat64 >> < aNumber [ 
	"Primitive. Compare the receiver with the argument and return true
	if the receiver is less than the argument. Otherwise return false.
	Fail if the argument is not a Float. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 543>
	^ aNumber adaptToFloat: self andCompare: #<
]

{ #category : #comparing }
SmallFloat64 >> <= aNumber [ 
	"Primitive. Compare the receiver with the argument and return true
	if the receiver is less than or equal to the argument. Otherwise return
	false. Fail if the argument is not a Float. Optional. See Object
	documentation whatIsAPrimitive."

	<primitive: 545>
	^ aNumber adaptToFloat: self andCompare: #<=
]

{ #category : #comparing }
SmallFloat64 >> = aNumber [ 
	"Primitive. Compare the receiver with the argument and return true
	if the receiver is equal to the argument. Otherwise return false.
	Fail if the argument is not a Float. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 547>
	aNumber isNumber ifFalse: [^ false].
	^ aNumber adaptToFloat: self andCompare: #=
]

{ #category : #comparing }
SmallFloat64 >> > aNumber [ 
	"Primitive. Compare the receiver with the argument and return true
	if the receiver is greater than the argument. Otherwise return false.
	Fail if the argument is not a Float. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 544>
	^ aNumber adaptToFloat: self andCompare: #>
]

{ #category : #comparing }
SmallFloat64 >> >= aNumber [ 
	"Primitive. Compare the receiver with the argument and return true
	if the receiver is greater than or equal to the argument. Otherwise return
	false. Fail if the argument is not a Float. Optional. See Object documentation 
	whatIsAPrimitive. "

	<primitive: 546>
	^ aNumber adaptToFloat: self andCompare: #>=
]

{ #category : #'mathematical functions' }
SmallFloat64 >> arcTan [
	"Answer the angle in radians.
	 Optional. See Object documentation whatIsAPrimitive."

	| theta eps step sinTheta cosTheta |
	<primitive: 557>

	"Newton-Raphson"
	self < 0.0 ifTrue: [ ^ 0.0 - (0.0 - self) arcTan ].

	"first guess"
	theta := (self * Halfpi) / (self + 1.0).

	"iterate"
	eps := Halfpi * Epsilon.
	step := theta.
	[(step * step) > eps] whileTrue: [
		sinTheta := theta sin.
		cosTheta := theta cos.
		step := (sinTheta * cosTheta) - (self * cosTheta * cosTheta).
		theta := theta - step].
	^ theta
]

{ #category : #comparing }
SmallFloat64 >> basicIdentityHash [
	"Answer an integer unique to the receiver."
	"primitive 171 is the primitiveImmediateAsInteger primitive that converts SmallIntegers to themselves, Characters to their integer codes, and SmallFloat64 to an integer representing their bits.  e.g.

#(-0.0 0.0 -1.0 1.0) collect: [:n| n -> n identityHash hex -> (n identityHash bitAnd: 2 << 64 - 1) hex]
{-0.0->'-16r1000000000000000'->'16r1F000000000000000' .
  0.0->'16r0'->'16r0' .
 -1.0->'-16r810000000000000'->'16r1F7F0000000000000' .
  1.0->'16r7F0000000000000'->'16r7F0000000000000'}"

	<primitive: 171>
	^self primitiveFailed
]

{ #category : #copying }
SmallFloat64 >> clone [
	"Answer the receiver, because SmallFloat64s are unique."
	^self
]

{ #category : #copying }
SmallFloat64 >> copy [
	"Answer the receiver, because SmallFloat64s are unique."
	^self
]

{ #category : #copying }
SmallFloat64 >> deepCopy [
	"Answer the receiver, because SmallFloat64s are unique."
	^self
]

{ #category : #'mathematical functions' }
SmallFloat64 >> exp [
	"Answer E raised to the receiver power.
	 Optional. See Object documentation whatIsAPrimitive." 

	| base fract correction delta div |
	<primitive: 559>

	"Taylor series"
	"check the special cases"
	self < 0.0 ifTrue: [^ (self negated exp) reciprocal].
	self = 0.0 ifTrue: [^ 1].
	self abs > MaxValLn ifTrue: [self error: 'exp overflow'].

	"get first approximation by raising e to integer power"
	base := E raisedToInteger: (self truncated).

	"now compute the correction with a short Taylor series"
	"fract will be 0..1, so correction will be 1..E"
	"in the worst case, convergance time is logarithmic with 1/Epsilon"
	fract := self fractionPart.
	fract = 0.0 ifTrue: [ ^ base ].  "no correction required"

	correction := 1.0 + fract.
	delta := fract * fract / 2.0.
	div := 2.0.
	[delta > Epsilon] whileTrue: [
		correction := correction + delta.
		div := div + 1.0.
		delta := delta * fract / div].
	correction := correction + delta.
	^ base * correction
]

{ #category : #'truncation and round off' }
SmallFloat64 >> exponent [
	"Primitive. Consider the receiver to be represented as a power of two
	multiplied by a mantissa (between one and two). Answer with the
	SmallInteger to whose power two is raised. Optional. See Object
	documentation whatIsAPrimitive."

	| positive |
	<primitive: 553>
	self >= 1.0 ifTrue: [^self floorLog: 2].
	self > 0.0
		ifTrue: 
			[positive := (1.0 / self) exponent.
			self = (1.0 / (1.0 timesTwoPower: positive))
				ifTrue: [^positive negated]
				ifFalse: [^positive negated - 1]].
	self = 0.0 ifTrue: [^-1].
	^self negated exponent
]

{ #category : #'truncation and round off' }
SmallFloat64 >> fractionPart [
	"Primitive. Answer a Float whose value is the difference between the 
	receiver and the receiver's asInteger value. Optional. See Object 
	documentation whatIsAPrimitive."

	<primitive: 552>
	^self - self truncated asFloat
]

{ #category : #comparing }
SmallFloat64 >> identityHash [
	"We need the override since basicIdentityHash is already in SmallInteger range,
	 the bitShift: 8 of the super implementation would lead to a LargeInteger"
	^ self basicIdentityHash
]

{ #category : #'mathematical functions' }
SmallFloat64 >> ln [
	"Answer the natural logarithm of the receiver.
	 Optional. See Object documentation whatIsAPrimitive."

	| expt n mant x div pow delta sum eps |
	<primitive: 558>

	"Taylor series"
	self <= 0.0 ifTrue: [^DomainError signal: 'ln is only defined for x > 0' from: 0].

	"get a rough estimate from binary exponent"
	expt := self exponent.
	n := Ln2 * expt.
	mant := self timesTwoPower: 0 - expt.

	"compute fine correction from mantinssa in Taylor series"
	"mant is in the range [0..2]"
	"we unroll the loop to avoid use of abs"
	x := mant - 1.0.
	div := 1.0.
	pow := delta := sum := x.
	x := x negated.  "x <= 0"
	eps := Epsilon * (n abs + 1.0).
	[delta > eps] whileTrue: [
		"pass one: delta is positive"
		div := div + 1.0.
		pow := pow * x.
		delta := pow / div.
		sum := sum + delta.
		"pass two: delta is negative"
		div := div + 1.0.
		pow := pow * x.
		delta := pow / div.
		sum := sum + delta].

	^ n + sum

	"2.718284 ln 1.0"
]

{ #category : #copying }
SmallFloat64 >> shallowCopy [
	"Answer the receiver, because SmallFloat64s are unique."
	^self
]

{ #category : #'mathematical functions' }
SmallFloat64 >> sin [
	"Answer the sine of the receiver taken as an angle in radians.
	 Optional. See Object documentation whatIsAPrimitive."

	| sum delta self2 i |
	<primitive: 556>

	"Taylor series"
	"normalize to the range [0..Pi/2]"
	self < 0.0 ifTrue: [^ (0.0 - ((0.0 - self) sin))].
	self > Twopi ifTrue: [^ (self \\ Twopi) sin].
	self > Pi ifTrue: [^ (0.0 - (self - Pi) sin)].
	self > Halfpi ifTrue: [^ (Pi - self) sin].

	"unroll loop to avoid use of abs"
	sum := delta := self.
	self2 := 0.0 - (self * self).
	i := 2.0.
	[delta > Epsilon] whileTrue: [
		"once"
		delta := (delta * self2) / (i * (i + 1.0)).
		i := i + 2.0.
		sum := sum + delta.
		"twice"
		delta := (delta * self2) / (i * (i + 1.0)).
		i := i + 2.0.
		sum := sum + delta].
	^ sum
]

{ #category : #'mathematical functions' }
SmallFloat64 >> sqrt [
	"Answer the square root of the receiver. 
	 Optional. See Object documentation whatIsAPrimitive."
	| exp guess eps delta |
	<primitive: 555>
	"Newton-Raphson"
	self <= 0.0
		ifTrue: [self = 0.0
				ifTrue: [^ 0.0]
				ifFalse: ["v Chg"
					^ DomainError signal: 'sqrt undefined for number less than zero.']].
	"first guess is half the exponent"
	exp := self exponent // 2.
	guess := self timesTwoPower: 0 - exp.
	"get eps value"
	eps := guess * Epsilon.
	eps := eps * eps.
	delta := self - (guess * guess) / (guess * 2.0).
	[delta * delta > eps]
		whileTrue: 
			[guess := guess + delta.
			delta := self - (guess * guess) / (guess * 2.0)].
	^ guess
]

{ #category : #'mathematical functions' }
SmallFloat64 >> timesTwoPower: anInteger [ 
	"Primitive. Answer with the receiver multiplied by 2.0 raised
	to the power of the argument.
	Optional. See Object documentation whatIsAPrimitive."

	<primitive: 554>

	anInteger < -29 ifTrue: [^ self * (2.0 raisedToInteger: anInteger)].
	anInteger < 0 ifTrue: [^ self / (1 bitShift: (0 - anInteger)) asFloat].
	anInteger < 30 ifTrue: [^ self * (1 bitShift: anInteger) asFloat].
	^ self * (2.0 raisedToInteger: anInteger)
]

{ #category : #'truncation and round off' }
SmallFloat64 >> truncated [
	"Answer with a SmallInteger equal to the value of the receiver without 
	its fractional part. The primitive fails if the truncated value cannot be 
	represented as a SmallInteger. In that case, the code below will compute 
	a LargeInteger truncated value.
	Essential. See Object documentation whatIsAPrimitive. "

	<primitive: 551>
	(self isInfinite or: [self isNaN]) ifTrue: [self error: 'Cannot truncate this number'].

	self abs < 2.0e16
		ifTrue: ["Fastest way when it may not be an integer"
				"^ (self quo: 1073741823.0) * 1073741823 + (self rem: 1073741823.0) truncated"
				| di df q r |
				di := (SmallInteger maxVal bitShift: -1)+1.
				df := di asFloat.
				q := self quo: df.
				r := self - (q asFloat * df).
				^q*di+r truncated]
		ifFalse: [^ self asTrueFraction.  "Extract all bits of the mantissa and shift if necess"]

		

		
]

{ #category : #copying }
SmallFloat64 >> veryDeepCopyWith: deepCopier [
	"Answer the receiver, because SmallFloat64s are unique."
	^self
]

{ #category : #comparing }
SmallFloat64 >> ~= aNumber [ 
	"Primitive. Compare the receiver with the argument and return true
	if the receiver is not equal to the argument. Otherwise return false.
	Fail if the argument is not a Float. Optional. See Object documentation
	whatIsAPrimitive."

	<primitive: 548>
	^super ~= aNumber
]
