"
I represent integers of more than 30 bits.  These values are beyond the range of SmallInteger, and are encoded here as an array of 8-bit digits. 
"
Class {
	#name : #LargeInteger,
	#superclass : #Integer,
	#type : #bytes,
	#category : #'Kernel-Numbers'
}

{ #category : #arithmetic }
LargeInteger >> * anInteger [ 
	"Primitive. Multiply the receiver by the argument and answer with an
	Integer result. Fail if either the argument or the result is not a
	SmallInteger or a LargePositiveInteger less than 2-to-the-30th (1073741824). Optional. See
	Object documentation whatIsAPrimitive. "

	<primitive: 29>
	^super * anInteger
]

{ #category : #arithmetic }
LargeInteger >> + anInteger [ 
	"Primitive. Add the receiver to the argument and answer with an
	Integer result. Fail if either the argument or the result is not a
	SmallInteger or a LargePositiveInteger less than 2-to-the-30th (1073741824). Optional. See
	Object documentation whatIsAPrimitive."

	<primitive: 21>
	^super + anInteger
]

{ #category : #arithmetic }
LargeInteger >> - anInteger [ 
	"Primitive. Subtract the argument from the receiver and answer with an
	Integer result. Fail if either the argument or the result is not a
	SmallInteger or a LargePositiveInteger less than 2-to-the-30th (1073741824). Optional. See
	Object documentation whatIsAPrimitive."

	<primitive: 22>
	^super - anInteger
]

{ #category : #arithmetic }
LargeInteger >> / anInteger [ 
	"Primitive. Divide the receiver by the argument and answer with the
	result if the division is exact. Fail if the result is not a whole integer.
	Fail if the argument is 0. Fail if either the argument or the result is not
	a SmallInteger or a LargePositiveInteger less than 2-to-the-30th (1073741824). Optional. See
	Object documentation whatIsAPrimitive. "

	<primitive: 30>
	^super / anInteger
]

{ #category : #arithmetic }
LargeInteger >> // anInteger [ 
	"Primitive. Divide the receiver by the argument and return the result.
	Round the result down towards negative infinity to make it a whole
	integer. Fail if the argument is 0. Fail if either the argument or the
	result is not a SmallInteger or a LargePositiveInteger less than 2-to-the-30th (1073741824).
	Optional. See Object documentation whatIsAPrimitive. "

	<primitive: 32>
	^super // anInteger
]

{ #category : #comparing }
LargeInteger >> < anInteger [ 
	"Primitive. Compare the receiver with the argument and answer true if
	the receiver is less than the argument. Otherwise answer false. Fail if the
	argument is not a SmallInteger or a LargePositiveInteger less than 2-to-the-30th (1073741824).
	Optional. See Object documentation whatIsAPrimitive."

	<primitive: 23>
	^super < anInteger
]

{ #category : #comparing }
LargeInteger >> <= anInteger [ 
	"Primitive. Compare the receiver with the argument and answer true if
	the receiver is less than or equal to the argument. Otherwise answer false.
	Fail if the argument is not a SmallInteger or a LargePositiveInteger less
	than 2-to-the-30th (1073741824). Optional. See Object documentation whatIsAPrimitive."

	<primitive: 25>
	^super <= anInteger
]

{ #category : #comparing }
LargeInteger >> > anInteger [ 
	"Primitive. Compare the receiver with the argument and answer true if
	the receiver is greater than the argument. Otherwise answer false. Fail if
	the argument is not a SmallInteger or a LargePositiveInteger less than
	2-to-the-30th (1073741824). Optional. See Object documentation whatIsAPrimitive."

	<primitive: 24>
	^super > anInteger
]

{ #category : #comparing }
LargeInteger >> >= anInteger [ 
	"Primitive. Compare the receiver with the argument and answer true if
	the receiver is greater than or equal to the argument. Otherwise answer
	false. Fail if the argument is not a SmallInteger or a LargePositiveInteger
	less than 2-to-the-30th (1073741824). Optional. See Object documentation whatIsAPrimitive."

	<primitive: 26>
	^super >= anInteger
]

{ #category : #arithmetic }
LargeInteger >> \\ aNumber [ 
	"Primitive. Take the receiver modulo the argument. The result is the
	remainder rounded towards negative infinity, of the receiver divided
	by the argument. Fail if the argument is 0. Fail if either the argument
	or the result is not a SmallInteger or a LargePositiveInteger less than
	2-to-the-30th (1073741824). Optional. See Object documentation whatIsAPrimitive."

	<primitive: 31>
	aNumber isInteger
		ifTrue:
			[| neg qr q r |
			neg := self negative == aNumber negative == false.
			qr := self digitDiv: aNumber neg: neg.
			q := qr first normalize.
			r := qr last normalize.
			^(q negative
				ifTrue: [r isZero not]
				ifFalse: [q isZero and: [neg]])
					ifTrue: [r + aNumber]
					ifFalse: [r]].
	^super \\ aNumber
	
]

{ #category : #arithmetic }
LargeInteger >> \\\ anInteger [ 
	"a faster modulo method for use in DSA. Be careful if you try to use this elsewhere"

	^(self digitDiv: anInteger neg: false) second
]

{ #category : #converting }
LargeInteger >> as31BitSmallInt [
	"This is only for 31 bit numbers.  Keep my 31 bits the same, but put them in a small int.  The small int will be negative since my 31st bit is 1.  We know my 31st bit is 1 because otherwise I would already be a positive small int."

	self highBit = 31 ifFalse: [self error: 'more than 31 bits can not fit in a SmallInteger'].

	^ self - 16r80000000
]

{ #category : #converting }
LargeInteger >> asFloat [
	"Answer a Float that best approximates the value of the receiver.
	This algorithm is optimized to process only the significant digits of a LargeInteger.
	And it does honour IEEE 754 round to nearest even mode in case of excess precision (see details below)."
	
	"How numbers are rounded in IEEE 754 default rounding mode:
	A shift is applied so that the highest 53 bits are placed before the floating point to form a mantissa.
	The trailing bits form the fraction part placed after the floating point.
	This fractional number must be rounded to the nearest integer.
	If fraction part is 2r0.1, exactly between two consecutive integers, there is a tie.
	The nearest even integer is chosen in this case.
	Examples (First 52bits of mantissa are omitted for brevity):
	2r0.00001 is rounded downward to 2r0
	2r1.00001 is rounded downward to 2r1
	2r0.1 is a tie and rounded to 2r0 (nearest even)
	2r1.1 is a tie and rounded to 2r10 (nearest even)
	2r0.10001 is rounded upward to 2r1
	2r1.10001 is rounded upward to 2r10
	Thus, if the next bit after floating point is 0, the mantissa is left unchanged.
	If next bit after floating point is 1, an odd mantissa is always rounded upper.
	An even mantissa is rounded upper only if the fraction part is not a tie."
	
	"Algorihm details:
	The floating point hardware can perform the rounding correctly with several excess bits as long as there is a single inexact operation.
	This can be obtained by splitting the mantissa plus excess bits in two part with less bits than Float precision.
	Note 1: the inexact flag in floating point hardware must not be trusted because in some cases the operations would be exact but would not take into account some bits that were truncated before the Floating point operations.
	Note 2: the floating point hardware is presumed configured in default rounding mode."
	
	| mantissa shift excess result n |

	"Check how many bits excess the maximum precision of a Float mantissa."
	excess := self highBitOfMagnitude - Float precision.
	excess > 7
		ifTrue:
			["Remove the excess bits but seven."
			mantissa := self bitShiftMagnitude: 7 - excess.
			shift := excess - 7.
			"An even mantissa with a single excess bit immediately following would be truncated.
			But this would not be correct if above shift has truncated some extra bits.
			Check this case, and round excess bits upper manually."
			((mantissa byteAt: 1) = 2r01000000 and: [self anyBitOfMagnitudeFrom: 1 to: shift])
				ifTrue: [mantissa := mantissa + 1]]
		ifFalse:
			[mantissa := self.
			shift := 0].

	"There will be a single inexact round off at last iteration"
	result := (mantissa byteAt: (n := mantissa bytesCount)) asFloat.
	[(n := n - 1) > 0] whileTrue: [
		result := 256.0 * result + (mantissa byteAt: n) asFloat].
	^result timesTwoPower: shift.
]

{ #category : #'system primitives' }
LargeInteger >> byteAt: index [ 
	"Primitive. Answer the value of an indexable field in the receiver.   LargePositiveInteger uses bytes of base two number, and each is a 'digit' base 256.  Fail if the argument (the index) is not an Integer or is out of bounds. Essential.  See Object documentation whatIsAPrimitive."

	<primitive: 60>
	self bytesCount < index
		ifTrue: [^0]
		ifFalse: [^super at: index]
]

{ #category : #'system primitives' }
LargeInteger >> byteAt: index put: value [ 
	"Primitive. Store the second argument (value) in the indexable field of 
	the receiver indicated by index. Fail if the value is negative or is larger 
	than 255. Fail if the index is not an Integer or is out of bounds. Answer 
	the value that was stored. Essential. See Object documentation 
	whatIsAPrimitive."

	<primitive: 61>
	^super at: index put: value
]

{ #category : #'system primitives' }
LargeInteger >> bytesCount [
	"Primitive. Answer the number of indexable fields in the receiver. This 
	value is the same as the largest legal subscript. Essential. See Object 
	documentation whatIsAPrimitive."

	<primitive: 62>
	self primitiveFailed
]

{ #category : #comparing }
LargeInteger >> hash [
	^ self bytesCount <= 8
		ifTrue: [ self ]
		ifFalse: [ ByteArray hashBytes: self startingWith: self species hash ]
]

{ #category : #'bit manipulation' }
LargeInteger >> hashMultiply [
	"Truncate to 28 bits and try again"

	^(self bitAnd: 16rFFFFFFF) hashMultiply
]

{ #category : #'bit manipulation' }
LargeInteger >> highBitOfMagnitude [
	"Answer the index of the high order bit of the magnitude of the  
	receiver, or zero if the receiver is zero.  
	This method is used for LargeNegativeIntegers as well,  
	since LargeIntegers are sign/magnitude."
	| realLength lastDigit |
	realLength := self bytesCount.
	[(lastDigit := self byteAt: realLength) = 0]
		whileTrue: [(realLength := realLength - 1) = 0 ifTrue: [^ 0]].
	^ lastDigit highBitOfPositiveReceiver + (8 * (realLength - 1))
]

{ #category : #testing }
LargeInteger >> isLarge [
	^true
]

{ #category : #testing }
LargeInteger >> mightBeASquare [
	self subclassResponsibility
]

{ #category : #printing }
LargeInteger >> printOn: aStream base: b nDigits: n [
	"Append a representation of this number in base b on aStream using n digits.
	In order to reduce cost of LargePositiveInteger ops, split the number of digts approximatily in two
	Should be invoked with: 0 <= self < (b raisedToInteger: n)"
	
	| halfPower half head tail |
	n <= 1 ifTrue: [
		n <= 0 ifTrue: [self error: 'Number of digits n should be > 0'].
		
		"Note: this is to stop an infinite loop if one ever attempts to print with a huge base
		This can happen because choice was to not hardcode any limit for base b
		We let Character>>#digitValue: fail"
		^aStream nextPut: (Character digitValue: self)].
	halfPower := n bitShift: -1.
	half := b raisedToInteger: halfPower.
	head := self quo: half.
	tail := self - (head * half).
	head printOn: aStream base: b nDigits: n - halfPower.
	tail printOn: aStream base: b nDigits: halfPower
]

{ #category : #arithmetic }
LargeInteger >> quo: anInteger [ 
	"Primitive. Divide the receiver by the argument and return the result.
	Round the result down towards zero to make it a whole integer. Fail if
	the argument is 0. Fail if either the argument or the result is not a
	SmallInteger or a LargePositiveInteger less than 2-to-the-30th (1073741824). Optional. See
	Object documentation whatIsAPrimitive."

	<primitive: 33>
	^super quo: anInteger
]

{ #category : #arithmetic }
LargeInteger >> rem: aNumber [ 
	"Remainder defined in terms of quo:. See super rem:.
	This is defined only to speed up case of large integers."

	<primitive: 20>
	 aNumber isInteger
		ifTrue:
			[| ng rem |
			ng := self negative == aNumber negative == false.
			rem := (self digitDiv: aNumber neg: ng) at: 2.
			^ rem normalize].
	^super rem: aNumber
]

{ #category : #'system primitives' }
LargeInteger >> replaceFrom: start to: stop with: replacement startingAt: repStart [ 
	"Primitive. This destructively replaces elements from start to stop in the receiver starting at index, repStart, in the collection, replacement. Answer the receiver. Range checks are performed in the primitive only. Optional. See Object documentation whatIsAPrimitive."
	<primitive: 105>
	^ super replaceFrom: start to: stop with: replacement startingAt: repStart
]

{ #category : #converting }
LargeInteger >> withAtLeastNDigits: desiredLength [

	| new |

	self size >= desiredLength ifTrue: [^self].
	new := self class new: desiredLength.
	new
		replaceFrom: 1 
		to: self size 
		with: self 
		startingAt: 1.
	^new
]
