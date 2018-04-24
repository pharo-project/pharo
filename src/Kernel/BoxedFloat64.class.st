"
My instances hold 64-bit Floats in heap objects.  This is the only representation on 32-bit systems.  But on 64-bit systems SmallFloat64 holds a subset of the full 64-bit double-precision range in immediate objects.
"
Class {
	#name : #BoxedFloat64,
	#superclass : #Float,
	#type : #words,
	#category : #'Kernel-Numbers'
}

{ #category : #'instance creation' }
BoxedFloat64 class >> basicNew [
	^self basicNew: 2
]

{ #category : #'instance creation' }
BoxedFloat64 class >> basicNew: sizeRequested [ 
	"Primitive. Answer an instance of this class with the number
	 of indexable variables specified by the argument, sizeRequested.
	 Fail if this class is not indexable or if the argument is not a
	 positive Integer, or if there is not enough memory available. 
	 Essential. See Object documentation whatIsAPrimitive."

	<primitive: 71>
	sizeRequested isInteger ifTrue:
		[^sizeRequested = 2
			ifTrue: "arg okay; space must be low."
				[OutOfMemory signal.
				 self basicNew: sizeRequested]  "retry if user proceeds"
			ifFalse:
				[self error: 'a Float shall always have two slots']].
	self primitiveFailed
]

{ #category : #arithmetic }
BoxedFloat64 >> * aNumber [ 
	"Primitive. Answer the result of multiplying the receiver by aNumber.
	Fail if the argument is not a Float. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 49>
	^ aNumber adaptToFloat: self andSend: #*
]

{ #category : #arithmetic }
BoxedFloat64 >> + aNumber [ 
	"Primitive. Answer the sum of the receiver and aNumber. Essential.
	Fail if the argument is not a Float. See Object documentation
	whatIsAPrimitive."

	<primitive: 41>
	^ aNumber adaptToFloat: self andSend: #+
]

{ #category : #arithmetic }
BoxedFloat64 >> - aNumber [ 
	"Primitive. Answer the difference between the receiver and aNumber.
	Fail if the argument is not a Float. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 42>
	^ aNumber adaptToFloat: self andSend: #-
]

{ #category : #arithmetic }
BoxedFloat64 >> / aNumber [ 
	"Primitive. Answer the result of dividing receiver by aNumber.
	Fail if the argument is not a Float. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 50>
	aNumber = 0.0 ifTrue: [ ^ ZeroDivide signalWithDividend: self].
	^aNumber adaptToFloat: self andSend: #/
]

{ #category : #comparing }
BoxedFloat64 >> < aNumber [ 
	"Primitive. Compare the receiver with the argument and return true
	if the receiver is less than the argument. Otherwise return false.
	Fail if the argument is not a Float. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 43>
	^ aNumber adaptToFloat: self andCompare: #<
]

{ #category : #comparing }
BoxedFloat64 >> <= aNumber [ 
	"Primitive. Compare the receiver with the argument and return true
	if the receiver is less than or equal to the argument. Otherwise return
	false. Fail if the argument is not a Float. Optional. See Object
	documentation whatIsAPrimitive."

	<primitive: 45>
	^ aNumber adaptToFloat: self andCompare: #<=
]

{ #category : #comparing }
BoxedFloat64 >> = aNumber [ 
	"Primitive. Compare the receiver with the argument and return true
	if the receiver is equal to the argument. Otherwise return false.
	Fail if the argument is not a Float. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 47>
	aNumber isNumber ifFalse: [^ false].
	^ aNumber adaptToFloat: self andCompare: #=
]

{ #category : #comparing }
BoxedFloat64 >> > aNumber [ 
	"Primitive. Compare the receiver with the argument and return true
	if the receiver is greater than the argument. Otherwise return false.
	Fail if the argument is not a Float. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 44>
	^ aNumber adaptToFloat: self andCompare: #>
]

{ #category : #comparing }
BoxedFloat64 >> >= aNumber [ 
	"Primitive. Compare the receiver with the argument and return true
	if the receiver is greater than or equal to the argument. Otherwise return
	false. Fail if the argument is not a Float. Optional. See Object documentation 
	whatIsAPrimitive. "

	<primitive: 46>
	^ aNumber adaptToFloat: self andCompare: #>=
]

{ #category : #'math functions' }
BoxedFloat64 >> exp [
	"Answer E raised to the receiver power.
	 Optional. See Object documentation whatIsAPrimitive." 

	| base fract correction delta div |
	<primitive: 59>

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

{ #category : #arithmetic }
BoxedFloat64 >> exponent [
	"Primitive. Consider the receiver to be represented as a power of two
	multiplied by a mantissa (between one and two). Answer with the
	SmallInteger to whose power two is raised. Optional. See Object
	documentation whatIsAPrimitive."

	| positive |
	<primitive: 53>
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
BoxedFloat64 >> fractionPart [
	"Primitive. Answer a Float whose value is the difference between the 
	receiver and the receiver's asInteger value. Optional. See Object 
	documentation whatIsAPrimitive."

	<primitive: 52>
	^self - self truncated asFloat
]

{ #category : #'math functions' }
BoxedFloat64 >> ln [
	"Answer the natural logarithm of the receiver.
	 Optional. See Object documentation whatIsAPrimitive."

	| expt n mant x div pow delta sum eps |
	<primitive: 58>

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

{ #category : #'math functions' }
BoxedFloat64 >> timesTwoPower: anInteger [ 
	"Primitive. Answer with the receiver multiplied by 2.0 raised
	to the power of the argument.
	Optional. See Object documentation whatIsAPrimitive."

	<primitive: 54>

	anInteger < -29 ifTrue: [^ self * (2.0 raisedToInteger: anInteger)].
	anInteger < 0 ifTrue: [^ self / (1 bitShift: (0 - anInteger)) asFloat].
	anInteger < 30 ifTrue: [^ self * (1 bitShift: anInteger) asFloat].
	^ self * (2.0 raisedToInteger: anInteger)
]

{ #category : #'truncation and round off' }
BoxedFloat64 >> truncated [
	"Answer with a SmallInteger equal to the value of the receiver without 
	its fractional part. The primitive fails if the truncated value cannot be 
	represented as a SmallInteger. In that case, the code below will compute 
	a LargeInteger truncated value.
	Essential. See Object documentation whatIsAPrimitive. "

	<primitive: 51>
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

{ #category : #comparing }
BoxedFloat64 >> ~= aNumber [ 
	"Primitive. Compare the receiver with the argument and return true
	if the receiver is not equal to the argument. Otherwise return false.
	Fail if the argument is not a Float. Optional. See Object documentation
	whatIsAPrimitive."

	<primitive: 48>
	^super ~= aNumber
]
