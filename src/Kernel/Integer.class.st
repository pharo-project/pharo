"
I am a common abstract superclass for all Integer implementations. My implementation subclasses are SmallInteger, LargePositiveInteger, and LargeNegativeInteger.
	
Integer division consists of:
	/	exact division, answers a fraction if result is not a whole integer
	//	answers an Integer, rounded towards negative infinity
	\\	is modulo rounded towards negative infinity
	quo: truncated division, rounded towards zero
"
Class {
	#name : #Integer,
	#superclass : #Number,
	#category : #'Kernel-Numbers'
}

{ #category : #'instance creation' }
Integer class >> basicNew [

	self == Integer ifTrue: [
		^ self error: 'Integer is an abstract class.  Make a concrete subclass.'].
	^ super basicNew
]

{ #category : #'instance creation' }
Integer class >> byte1: byte1 byte2: byte2 byte3: byte3 byte4: byte4 [ 
	"Depending on high-order byte copy directly into a LargeInteger,
	or build up a SmallInteger by shifting"
	| value |
	byte4 < 16r40 ifTrue:
		[^ (byte4 bitShift: 24)
		 + (byte3 bitShift: 16)
		 + (byte2 bitShift: 8)
		 + byte1].
	value := LargePositiveInteger new: 4.
	value byteAt: 4 put: byte4.
	value byteAt: 3 put: byte3.
	value byteAt: 2 put: byte2.
	value byteAt: 1 put: byte1.
	^ value
]

{ #category : #testing }
Integer class >> isAbstract [

	^self name = #Integer
]

{ #category : #'instance creation' }
Integer class >> new [

	self == Integer ifTrue: [
		^ self error: 'Integer is an abstract class.  Make a concrete subclass.'].
	^ super new
]

{ #category : #'instance creation' }
Integer class >> new: length neg: neg [
	"Answer an instance of a large integer whose size is length. neg is a flag 
	determining whether the integer is negative or not."

	neg 
		ifTrue: [^LargeNegativeInteger new: length]
		ifFalse: [^LargePositiveInteger new: length]
]

{ #category : #reading }
Integer class >> readHexByteFrom: characterReadStream [
	| highNibble lowNibble |
	highNibble := characterReadStream next
		ifNotNil: [ :character | character hexDigitValue ].
	highNibble ifNil: [ self error: 'hex digit expected' ].
	lowNibble := characterReadStream next
		ifNotNil: [ :character | character hexDigitValue ].
	lowNibble ifNil: [ self error: 'hex digit expected' ].
	^ (highNibble bitShift: 4) + lowNibble
]

{ #category : #'bit manipulation' }
Integer >> & aNumber [
	^ self bitAnd: aNumber
]

{ #category : #arithmetic }
Integer >> * aNumber [
	"Refer to the comment in Number * " 
	aNumber isInteger ifTrue:
		[^ self digitMultiply: aNumber 
					neg: self negative ~~ aNumber negative].
	^ aNumber adaptToInteger: self andSend: #*
]

{ #category : #arithmetic }
Integer >> + aNumber [
	"Refer to the comment in Number + "
	aNumber isInteger ifTrue:
		[self negative == aNumber negative
			ifTrue: [^ (self digitAdd: aNumber) normalize]
			ifFalse: [^ self digitSubtract: aNumber]].
	aNumber isFraction ifTrue:
		[^Fraction numerator: self * aNumber denominator + aNumber numerator denominator: aNumber denominator].
	^ aNumber adaptToInteger: self andSend: #+
]

{ #category : #arithmetic }
Integer >> - aNumber [
	"Refer to the comment in Number - "
	aNumber isInteger ifTrue:
		[self negative == aNumber negative
			ifTrue: [^ self digitSubtract: aNumber]
			ifFalse: [^ (self digitAdd: aNumber) normalize]].
	aNumber isFraction ifTrue:
		[^Fraction numerator: self * aNumber denominator - aNumber numerator denominator: aNumber denominator].
	^ aNumber adaptToInteger: self andSend: #-
]

{ #category : #arithmetic }
Integer >> / aNumber [
	"Refer to the comment in Number / "
	| quoRem |
	aNumber isInteger ifTrue:
		[quoRem := self digitDiv: aNumber neg: self negative ~~ aNumber negative.
		(quoRem at: 2) = 0
			ifTrue: [^ (quoRem at: 1) normalize]
			ifFalse: [^ (Fraction numerator: self denominator: aNumber) reduced]].
	^ aNumber adaptToInteger: self andSend: #/
]

{ #category : #arithmetic }
Integer >> // aNumber [ 
	| q |
	aNumber = 0 ifTrue: [^ (ZeroDivide dividend: self) signal"<- Chg"].
	self = 0 ifTrue: [^ 0].
	q := self quo: aNumber.
	"Refer to the comment in Number|//."
	(q negative
		ifTrue: [q * aNumber ~= self]
		ifFalse: [q = 0 and: [self negative ~= aNumber negative]])
		ifTrue: [^ q - 1"Truncate towards minus infinity."]
		ifFalse: [^ q]
]

{ #category : #comparing }
Integer >> < aNumber [
	aNumber isInteger ifTrue:
		[self negative == aNumber negative
			ifTrue: [self negative
						ifTrue: [^ (self bytesCompare: aNumber) > 0]
						ifFalse: [^ (self bytesCompare: aNumber) < 0]]
			ifFalse: [^ self negative]].
	^ aNumber adaptToInteger: self andCompare: #<
]

{ #category : #'bit manipulation' }
Integer >> << shiftAmount [
	"left shift"
	
	shiftAmount < 0 ifTrue: [self error: 'negative arg'].
	^ self bitShift: shiftAmount
]

{ #category : #comparing }
Integer >> <= aNumber [
	aNumber isInteger ifTrue:
		[self negative == aNumber negative
			ifTrue: [self negative
						ifTrue: [^ (self bytesCompare: aNumber) >= 0]
						ifFalse: [^ (self bytesCompare: aNumber) <= 0]]
			ifFalse: [^ self negative]].
	^ aNumber adaptToInteger: self andCompare: #<=
]

{ #category : #comparing }
Integer >> = aNumber [
	aNumber isNumber ifFalse: [^ false].
	aNumber isInteger ifTrue:
		[aNumber negative == self negative
			ifTrue: [^ (self bytesCompare: aNumber) = 0]
			ifFalse: [^ false]].
	^ aNumber adaptToInteger: self andCompare: #=
]

{ #category : #comparing }
Integer >> > aNumber [
	aNumber isInteger ifTrue:
		[self negative == aNumber negative
			ifTrue: [self negative
						ifTrue: [^(self bytesCompare: aNumber) < 0]
						ifFalse: [^(self bytesCompare: aNumber) > 0]]
			ifFalse: [^ aNumber negative]].
	^ aNumber adaptToInteger: self andCompare: #>
]

{ #category : #comparing }
Integer >> >= aNumber [
	aNumber isInteger ifTrue:
		[self negative == aNumber negative
			ifTrue: [self negative
						ifTrue: [^(self bytesCompare: aNumber) <= 0]
						ifFalse: [^(self bytesCompare: aNumber) >= 0]]
			ifFalse: [^ aNumber negative]].
	^ aNumber adaptToInteger: self andCompare: #>=
]

{ #category : #'bit manipulation' }
Integer >> >> shiftAmount [
	"right shift"
	
	shiftAmount < 0 ifTrue: [self error: 'negative arg'].
	^ self bitShift: 0 - shiftAmount
]

{ #category : #arithmetic }
Integer >> \\\ anInteger [ 
	"a modulo method for use in DSA. Be careful if you try to use this elsewhere."

	^self \\ anInteger
]

{ #category : #converting }
Integer >> adaptToFraction: rcvr andSend: selector [
	"If I am involved in arithmetic with a Fraction, convert me to a Fraction."
	^ rcvr perform: selector with: (Fraction numerator: self denominator: 1)
]

{ #category : #arithmetic }
Integer >> alignedTo: anInteger [
	"Answer the smallest number not less than receiver that is a multiple of anInteger."

	^(self+anInteger-1//anInteger)*anInteger

"5 alignedTo: 2"
"12 alignedTo: 3"
]

{ #category : #'bit manipulation' }
Integer >> allMask: mask [ 
	"Treat the argument as a bit mask. Answer whether all of the bits that 
	are 1 in the argument are 1 in the receiver."

	^mask = (self bitAnd: mask)
]

{ #category : #'bit manipulation' }
Integer >> anyBitOfMagnitudeFrom: start to: stopArg [ 
	"Tests for any magnitude bits in the interval from start to stopArg."
	"Primitive fixed in LargeIntegers v1.2. If you have an earlier version 
	comment out the primitive call (using this ST method then)."
	| magnitude firstDigitIx lastDigitIx rightShift leftShift stop |
	<primitive: 'primAnyBitFromTo' module:'LargeIntegers'>
	start < 1 | (stopArg < 1)
		ifTrue: [^ self error: 'out of range'].
	magnitude := self abs.
	stop := stopArg min: magnitude highBit.
	start > stop
		ifTrue: [^ false].
	firstDigitIx := start - 1 // 8 + 1.
	lastDigitIx := stop - 1 // 8 + 1.
	rightShift := (start - 1 \\ 8) negated.
	leftShift := 7 - (stop - 1 \\ 8).
	firstDigitIx = lastDigitIx
		ifTrue: [| digit mask | 
			mask := (255 bitShift: rightShift negated)
						bitAnd: (255 bitShift: leftShift negated).
			digit := magnitude byteAt: firstDigitIx.
			^ (digit bitAnd: mask)
				~= 0].
	((magnitude byteAt: firstDigitIx)
			bitShift: rightShift)
			~= 0
		ifTrue: [^ true].
	firstDigitIx + 1
		to: lastDigitIx - 1
		do: [:ix | (magnitude byteAt: ix)
					~= 0
				ifTrue: [^ true]].
	(((magnitude byteAt: lastDigitIx)
			bitShift: leftShift)
			bitAnd: 255)
			~= 0
		ifTrue: [^ true].
	^ false
]

{ #category : #'bit manipulation' }
Integer >> anyMask: mask [ 
	"Treat the argument as a bit mask. Answer whether any of the bits that 
	are 1 in the argument are 1 in the receiver."

	^0 ~= (self bitAnd: mask)
]

{ #category : #accessing }
Integer >> as31BitSmallInt [
	^ self subclassResponsibility
]

{ #category : #'converting-arrays' }
Integer >> asByteArray [

	| stream |
	stream := ByteArray new writeStream.
	self bytesCount to: 1 by: -1 do: [:digitIndex |
		stream nextPut: (self byteAt: digitIndex)].
	^ stream contents

]

{ #category : #'converting-arrays' }
Integer >> asByteArrayOfSize: aSize [ 
	"Answer a ByteArray of aSize with my value, most-significant byte first."
	| answer digitPos |
	aSize < self bytesCount ifTrue: [ self error: 'number to large for byte array' ].
	answer := ByteArray new: aSize.
	digitPos := 1.
	aSize 
		to: aSize - self bytesCount + 1
		by: -1
		do: 
			[ :pos | 
			answer 
				at: pos
				put: (self byteAt: digitPos).
			digitPos := digitPos + 1 ].
	^ answer
]

{ #category : #converting }
Integer >> asCharacter [
	"Answer the Character whose value is the receiver."
	^Character value: self
]

{ #category : #converting }
Integer >> asCharacterDigit [
	"Answer the Character whose string representation is the receiver."
	^Character digitValue: self
]

{ #category : #converting }
Integer >> asFraction [
	"Answer a Fraction that represents the value of the receiver.
	Since an Integer already behaves as a special kind of Fraction, no conversion is required, see #isFraction."

	^self
]

{ #category : #converting }
Integer >> asHexDigit [
	^'0123456789ABCDEF' at: self+1
]

{ #category : #converting }
Integer >> asInteger [
	"Answer with the receiver itself."

	^self


]

{ #category : #'truncation and round off' }
Integer >> asLargerPowerOfTwo [
	"Convert the receiver into a power of two which is not less than the receiver"
	^self isPowerOfTwo
		ifTrue:[self]
		ifFalse:[self > 0 ifTrue: [	1 bitShift: (self highBit)]
						ifFalse: [DomainError signal: 'Value outside (0 , infinity)' from: 0]]
]

{ #category : #'truncation and round off' }
Integer >> asPowerOfTwo [
	"Convert the receiver into a power of two"
	^self asSmallerPowerOfTwo
]

{ #category : #converting }
Integer >> asScaledDecimal [
	"The number of significant digits of the answer is the same as the number of decimal digits in the receiver."
	^ ScaledDecimal newFromNumber: self scale: 0
]

{ #category : #'*Kernel-Chronology' }
Integer >> asSeconds [
	^ Duration seconds: self
]

{ #category : #'truncation and round off' }
Integer >> asSmallerPowerOfTwo [
	"Convert the receiver into a power of two which is not larger than the receiver"
	^self isPowerOfTwo
		ifTrue:[self]
		ifFalse:[self > 0 ifTrue: [	1 bitShift: (self highBit - 1)]
						ifFalse: [DomainError signal: 'Value outside (0 , infinity)' from: 0]]
]

{ #category : #printing }
Integer >> asStringWithCommas [
	"123456789 asStringWithCommas"
	"-123456789 asStringWithCommas"
	^ String streamContents: [:stream | 
		self printWithCommasOn: stream ]
]

{ #category : #printing }
Integer >> asStringWithCommasSigned [
	"123456789 asStringWithCommasSigned"
	"-123456789 asStringWithCommasSigned"
	^ String streamContents: [:stream | 
		self printWithCommasSignedOn: stream ]
]

{ #category : #printing }
Integer >> asTwoCharacterString [
	"Answer a two-character string representing the receiver, with leading zero if required.  Intended for use with integers in the range 0 to 99, but plausible replies given for other values too
	Examples:
		2 asTwoCharacterString
		11 asTwoCharacterString
		1943 asTwoCharacterString
		0 asTwoCharacterString
		-2 asTwoCharacterString
		-234 asTwoCharacterString "

	^ (self >= 0 and: [self < 10])
		ifTrue:	['0', self printString]
		ifFalse:	[self printString copyFrom: 1 to: 2]
]

{ #category : #'bit manipulation' }
Integer >> bitAnd: n [ 
	"Answer an Integer whose bits are the logical AND of the receiver's bits  
	and those of the argument, n."
	| norm |
	<primitive: 'primDigitBitAnd' module:'LargeIntegers'>
	norm := n normalize.
	^ self
		digitLogic: norm
		op: #bitAnd:
		length: (self bytesCount max: norm bytesCount)
]

{ #category : #'bit manipulation' }
Integer >> bitAt: anInteger [
	"Answer 1 if the bit at position anInteger is set to 1, 0 otherwise.
	self is considered an infinite sequence of bits, so anInteger can be any strictly positive integer.
	Bit at position 1 is the least significant bit.
	Negative numbers are in two-complements.
	
	This is a naive implementation that can be refined in subclass for speed"
	
	^(self bitShift: 1 - anInteger) bitAnd: 1
]

{ #category : #'bit manipulation' }
Integer >> bitAt: anInteger put: value [
	"Answer a new Integer that has the bit of rank anInteger set to value.
	The bit value should be 0 or 1, otherwise raise an Error.
	The bits are indexed starting at 1 for the least significant bit.
	For negative integers, operate on 2-complement representation."
	
	| b |
	b := self bitAt: anInteger.
	b = value ifTrue: [^self].
	0 = value ifTrue: [^self bitAnd: (1 bitShift: anInteger - 1) bitInvert].
	1 = value ifTrue: [^self bitOr: (1 bitShift: anInteger - 1)].
	self error: 'bit value should be 0 or 1'
]

{ #category : #'bit manipulation' }
Integer >> bitClear: aMask [ 
	"Answer an Integer equal to the receiver, except with all bits cleared that are set in aMask."

	^ (self bitOr: aMask) - aMask
]

{ #category : #'bit manipulation' }
Integer >> bitInvert [
	"Answer an Integer whose bits are the logical negation of the receiver's bits.
	Numbers are interpreted as having 2's-complement representation."

	^ -1 - self.
]

{ #category : #'bit manipulation' }
Integer >> bitInvert32 [
	"Answer the 32-bit complement of the receiver."

	^ self bitXor: 16rFFFFFFFF
]

{ #category : #'bit manipulation' }
Integer >> bitOr: n [ 
	"Answer an Integer whose bits are the logical OR of the receiver's bits  
	and those of the argument, n."
	| norm |
	<primitive: 'primDigitBitOr' module:'LargeIntegers'>
	norm := n normalize.
	^ self
		digitLogic: norm
		op: #bitOr:
		length: (self bytesCount max: norm bytesCount)
]

{ #category : #'bit manipulation' }
Integer >> bitShift: shiftCount [ 
	"Answer an Integer whose value (in twos-complement representation) is  
	the receiver's value (in twos-complement representation) shifted left by 
	the number of bits indicated by the argument. Negative arguments  
	shift right. Zeros are shifted in from the right in left shifts."
	| magnitudeShift |
	magnitudeShift := self bitShiftMagnitude: shiftCount.
	^ ((self negative and: [shiftCount negative])
		and: [self anyBitOfMagnitudeFrom: 1 to: shiftCount negated])
		ifTrue: [magnitudeShift - 1]
		ifFalse: [magnitudeShift]
]

{ #category : #'bit manipulation' }
Integer >> bitShiftMagnitude: shiftCount [ 
	"Answer an Integer whose value (in magnitude representation) is  
	the receiver's value (in magnitude representation) shifted left by  
	the number of bits indicated by the argument. Negative arguments
	shift right. Zeros are shifted in from the right in left shifts."
	| rShift |
	<primitive: 'primDigitBitShiftMagnitude' module:'LargeIntegers'>
	shiftCount >= 0 ifTrue: [^ self digitLshift: shiftCount].
	rShift := 0 - shiftCount.
	^ (self
		digitRshift: (rShift bitAnd: 7)
		bytes: (rShift bitShift: -3)
		lookfirst: self bytesCount) normalize
]

{ #category : #'bit manipulation' }
Integer >> bitString [
	"Returns a string representing the receiver in binary form"
	"2 bitString 
		'0000000000000000000000000000010'
	
	-1 bitString	 
		'1111111111111111111111111111111'
	
	-2 bitString 
		'1111111111111111111111111111110'
	"

      ^(self bitStringLength to: 1 by: -1) 
		collect: [:i | Character value: $0 charCode + (self bitAt: i)] as: String
]

{ #category : #'bit manipulation' }
Integer >> bitStringLength [

      ^self bytesCount * 8
           	"make sure positive integer bitString always begins with 0"
           + (self positive ifTrue: [1] ifFalse: [0])
]

{ #category : #'bit manipulation' }
Integer >> bitXor: n [ 
	"Answer an Integer whose bits are the logical XOR of the receiver's bits  
	and those of the argument, n."
	| norm |
	<primitive: 'primDigitBitXor' module:'LargeIntegers'>
	norm := n normalize.
	^ self
		digitLogic: norm
		op: #bitXor:
		length: (self bytesCount max: norm bytesCount)
]

{ #category : #private }
Integer >> bytesCompare: arg [ 
	"Compare the magnitude of self with that of arg.   
	Return a code of 1, 0, -1 for self >, = , < arg"
	| len arglen argDigit selfDigit |
	<primitive: 'primDigitCompare' module:'LargeIntegers'>
	len := self bytesCount.
	(arglen := arg bytesCount) ~= len
		ifTrue: [arglen > len
				ifTrue: [^ -1]
				ifFalse: [^ 1]].
	[len > 0]
		whileTrue: 
			[(argDigit := arg byteAt: len) ~= (selfDigit := self byteAt: len)
				ifTrue: [argDigit < selfDigit
						ifTrue: [^ 1]
						ifFalse: [^ -1]].
			len := len - 1].
	^ 0
]

{ #category : #'system primitives' }
Integer >> bytesCount [
	self subclassResponsibility
]

{ #category : #'truncation and round off' }
Integer >> ceiling [ 
	"Refer to the comment in Number|ceiling."
]

{ #category : #private }
Integer >> copyto: x [
	| stop |
	stop := self bytesCount min: x bytesCount.
	^ x replaceFrom: 1 to: stop with: self startingAt: 1
]

{ #category : #arithmetic }
Integer >> crossSumBase: aBase [
	|aResult|
	"Precondition"
	[aBase isInteger and: [aBase >=2]] assert.

	self < 0 ifTrue: [^self negated crossSumBase: aBase].
	self < aBase ifTrue: [^ self].
	aResult := self \\ aBase + (self // aBase crossSumBase: aBase).

	"Postcondition
	E.g. 18 crossSumBase: 10 -> 9 => 18\\(10-1) = 0"
	[((aResult \\ (aBase - 1) = 0)) = ((self \\ (aBase - 1)) =0)] assert.
	^aResult
]

{ #category : #accessing }
Integer >> decimalDigitLength [

	"Return how many digits are necessary to print this number in base 10.
	This does not count any place for minus sign, radix prefix or whatever.
	Result is not defined from negative numbers."
	
	^ self numberOfDigitsInBase: 10
]

{ #category : #accessing }
Integer >> denominator [
	"Let an Integer be polymorphic to a Fraction. See #isFraction."
	^1
]

{ #category : #private }
Integer >> digitAdd: arg [ 
	| len arglen accum sum |
	<primitive: 'primDigitAdd' module:'LargeIntegers'>
	accum := 0.
	(len := self bytesCount) < (arglen := arg bytesCount) ifTrue: [len := arglen].
	"Open code max: for speed"
	sum := Integer new: len neg: self negative.
	1 to: len do: 
		[:i | 
		accum := (accum bitShift: -8)
					+ (self byteAt: i) + (arg byteAt: i).
		sum byteAt: i put: (accum bitAnd: 255)].
	accum > 255
		ifTrue: 
			[sum := sum growby: 1.
			sum at: sum bytesCount put: (accum bitShift: -8)].
	^ sum
]

{ #category : #accessing }
Integer >> digitAt: anExponent base: base [

	"Return number that represents digit at given position.
		42 digitAt: 2 base: 10 -> 4
		42 digitAt: 1 base: 10 -> 1
	It is always a number or zero:
		16rFF digitAt: 1 base: 16 -> 15
		1 digitAt: 2 base: 10 -> 0
	Results are not defined for base smaller than 2 and non-integer arguments."

	^ self // (base raisedToInteger: anExponent - 1) \\ base
]

{ #category : #private }
Integer >> digitDiv: arg neg: ng [ 
	"Answer with an array of (quotient, remainder)."
	| quo rem ql d div dh dnh dl qhi qlo j l hi lo r3 a t |
	<primitive: 'primDigitDivNegative' module:'LargeIntegers'>
	arg = 0 ifTrue: [^ (ZeroDivide dividend: self) signal].
	"TFEI added this line"
	l := self bytesCount - arg bytesCount + 1.
	l <= 0 ifTrue: [^ Array with: 0 with: self].
	"shortcut against #highBit"
	d := 8 - arg lastDigit highBitOfPositiveReceiver.
	div := arg digitLshift: d.
	div := div growto: div bytesCount + 1.
	"shifts so high order word is >=128"
	rem := self digitLshift: d.
	rem bytesCount = self bytesCount ifTrue: [rem := rem growto: self bytesCount + 1].
	"makes a copy and shifts"
	quo := Integer new: l neg: ng.
	dl := div bytesCount - 1.
	"Last actual byte of data"
	ql := l.
	dh := div byteAt: dl.
	dnh := dl = 1
				ifTrue: [0]
				ifFalse: [div byteAt: dl - 1].
	1 to: ql do: 
		[:k | 
		"maintain quo*arg+rem=self"
		"Estimate rem/div by dividing the leading to bytes of rem by dh."
		"The estimate is q = qhi*16+qlo, where qhi and qlo are nibbles."
		j := rem bytesCount + 1 - k.
		"r1 := rem digitAt: j."
		(rem byteAt: j)
			= dh
			ifTrue: [qhi := qlo := 15
				"i.e. q=255"]
			ifFalse: 
				["Compute q = (r1,r2)//dh, t = (r1,r2)\\dh.  
				Note that r1,r2 are bytes, not nibbles.  
				Be careful not to generate intermediate results exceeding 13  
				bits."
				"r2 := (rem digitAt: j - 1)."
				t := ((rem byteAt: j)
							bitShift: 4)
							+ ((rem byteAt: j - 1)
									bitShift: -4).
				qhi := t // dh.
				t := (t \\ dh bitShift: 4)
							+ ((rem byteAt: j - 1)
									bitAnd: 15).
				qlo := t // dh.
				t := t \\ dh.
				"Next compute (hi,lo) := q*dnh"
				hi := qhi * dnh.
				lo := qlo * dnh + ((hi bitAnd: 15)
								bitShift: 4).
				hi := (hi bitShift: -4)
							+ (lo bitShift: -8).
				lo := lo bitAnd: 255.
				"Correct overestimate of q.  
				Max of 2 iterations through loop -- see Knuth vol. 2"
				r3 := j < 3
							ifTrue: [0]
							ifFalse: [rem byteAt: j - 2].
				[(t < hi
					or: [t = hi and: [r3 < lo]])
					and: 
						["i.e. (t,r3) < (hi,lo)"
						qlo := qlo - 1.
						lo := lo - dnh.
						lo < 0
							ifTrue: 
								[hi := hi - 1.
								lo := lo + 256].
						hi >= dh]]
					whileTrue: [hi := hi - dh].
				qlo < 0
					ifTrue: 
						[qhi := qhi - 1.
						qlo := qlo + 16]].
		"Subtract q*div from rem"
		l := j - dl.
		a := 0.
		1 to: div bytesCount do: 
			[:i | 
			hi := (div byteAt: i)
						* qhi.
			lo := a + (rem byteAt: l) - ((hi bitAnd: 15)
							bitShift: 4) - ((div byteAt: i)
							* qlo).
			rem byteAt: l put: lo - (lo // 256 * 256).
			"sign-tolerant form of (lo bitAnd: 255)"
			a := lo // 256 - (hi bitShift: -4).
			l := l + 1].
		a < 0
			ifTrue: 
				["Add div back into rem, decrease q by 1"
				qlo := qlo - 1.
				l := j - dl.
				a := 0.
				1 to: div bytesCount do: 
					[:i | 
					a := (a bitShift: -8)
								+ (rem byteAt: l) + (div byteAt: i).
					rem byteAt: l put: (a bitAnd: 255).
					l := l + 1]].
		quo byteAt: quo bytesCount + 1 - k put: (qhi bitShift: 4)
				+ qlo].
	rem := rem
				digitRshift: d
				bytes: 0
				lookfirst: dl.
	^ Array with: quo with: rem
]

{ #category : #private }
Integer >> digitLogic: arg op: op length: len [ 
	| i result neg1 neg2 rneg z1 z2 rz b1 b2 b |
	neg1 := self negative.
	neg2 := arg negative.
	rneg := ((neg1
				ifTrue: [-1]
				ifFalse: [0])
				perform: op
				with: (neg2
						ifTrue: [-1]
						ifFalse: [0]))
				< 0.
	result := Integer new: len neg: rneg.
	rz := z1 := z2 := true.
	i := 0.
	[(i := i + 1) <= len
		or: ["mind a carry on result that might go past len digits"
			rneg and: [rz
				and: [result := result growby: 1.
					true]]]]
		whileTrue: [b1 := self byteAt: i.
			neg1
				ifTrue: [b1 := z1
								ifTrue: [b1 = 0
										ifTrue: [0]
										ifFalse: [z1 := false.
											256 - b1]]
								ifFalse: [255 - b1]].
			b2 := arg byteAt: i.
			neg2
				ifTrue: [b2 := z2
								ifTrue: [b2 = 0
										ifTrue: [0]
										ifFalse: [z2 := false.
											256 - b2]]
								ifFalse: [255 - b2]].
			b := b1 perform: op with: b2.
			result
				byteAt: i
				put: (rneg
						ifTrue: [rz
								ifTrue: [b = 0
										ifTrue: [0]
										ifFalse: [rz := false.
											256 - b]]
								ifFalse: [255 - b]]
						ifFalse: [b])].
	^ result normalize
]

{ #category : #private }
Integer >> digitLshift: shiftCount [ 
	| carry rShift mask len result digit byteShift bitShift highBit |
	(highBit := self highBitOfMagnitude) = 0 ifTrue: [^ 0].
	len := highBit + shiftCount + 7 // 8.
	result := Integer new: len neg: self negative.
	byteShift := shiftCount // 8.
	bitShift := shiftCount \\ 8.
	bitShift = 0 ifTrue: ["Fast version for byte-aligned shifts"
		^ result
			replaceFrom: byteShift + 1
			to: len
			with: self
			startingAt: 1].
	carry := 0.
	rShift := bitShift - 8.
	mask := 255 bitShift: 0 - bitShift.
	1 to: byteShift do: [:i | result byteAt: i put: 0].
	1 to: len - byteShift do: 
		[:i | 
		digit := self byteAt: i.
		result byteAt: i + byteShift put: (((digit bitAnd: mask)
				bitShift: bitShift)
				bitOr: carry).
		carry := digit bitShift: rShift].
	^ result
]

{ #category : #private }
Integer >> digitMultiply: arg neg: ng [ 
	| prod prodLen carry digit k ab |
	<primitive: 'primDigitMultiplyNegative' module:'LargeIntegers'>
	(arg bytesCount = 1 and: [(arg byteAt: 1)
			= 0])
		ifTrue: [^ 0].
	(self bytesCount = 1 and: [(self byteAt: 1)
			= 0])
		ifTrue: [^ 0].
	prodLen := self bytesCount + arg bytesCount.
	prod := Integer new: prodLen neg: ng.
	"prod starts out all zero"
	1 to: self bytesCount do: [:i | (digit := self byteAt: i) ~= 0
			ifTrue: 
				[k := i.
				carry := 0.
				"Loop invariant: 0<=carry<=0377, k=i+j-1"
				1 to: arg bytesCount do: 
					[:j | 
					ab := (arg byteAt: j)
								* digit + carry + (prod byteAt: k).
					carry := ab bitShift: -8.
					prod byteAt: k put: (ab bitAnd: 255).
					k := k + 1].
				prod byteAt: k put: carry]].
	^ prod normalize
]

{ #category : #private }
Integer >> digitRshift: anInteger bytes: b lookfirst: a [ 
	 "Shift right 8*b+anInteger bits, 0<=n<8.
	Discard all digits beyond a, and all zeroes at or below a."
	| n x r f m digit count i |
	n := 0 - anInteger.
	x := 0.
	f := n + 8.
	i := a.
	m := 255 bitShift: 0 - f.
	digit := self byteAt: i.
	[((digit bitShift: n) bitOr: x) = 0 and: [i ~= 1]] whileTrue:
		[x := digit bitShift: f "Can't exceed 8 bits".
		i := i - 1.
		digit := self byteAt: i].
	i <= b ifTrue: [^Integer new: 0 neg: self negative].  "All bits lost"
	r := Integer new: i - b neg: self negative.
	count := i.
	x := (self byteAt: b + 1) bitShift: n.
	b + 1 to: count do:
		[:j | digit := self byteAt: j + 1.
		r byteAt: j - b put: (((digit bitAnd: m) bitShift: f) bitOr: x) 
			"Avoid values > 8 bits".
		x := digit bitShift: n].
	^r
]

{ #category : #private }
Integer >> digitSubtract: arg [ 
	| smaller larger z sum sl al ng |
	<primitive: 'primDigitSubtract' module:'LargeIntegers'>
	sl := self bytesCount.
	al := arg bytesCount.
	(sl = al
		ifTrue: 
			[[(self byteAt: sl)
				= (arg byteAt: sl) and: [sl > 1]]
				whileTrue: [sl := sl - 1].
			al := sl.
			(self byteAt: sl)
				< (arg byteAt: sl)]
		ifFalse: [sl < al])
		ifTrue: 
			[larger := arg.
			smaller := self.
			ng := self negative == false.
			sl := al]
		ifFalse: 
			[larger := self.
			smaller := arg.
			ng := self negative].
	sum := Integer new: sl neg: ng.
	z := 0.
	"Loop invariant is -1<=z<=1"
	1 to: sl do: 
		[:i | 
		z := z + (larger byteAt: i) - (smaller byteAt: i).
		sum byteAt: i put: z - (z // 256 * 256).
		"sign-tolerant form of (z bitAnd: 255)"
		z := z // 256].
	^ sum normalize
]

{ #category : #accessing }
Integer >> digitSum [
    "Returns the digit sum of the receiver"
    
    ^self abs asString inject: 0 into: [:value :new| value + new digitValue]
]

{ #category : #testing }
Integer >> even [ 
	"Refer to the comment in Number|even."

	^((self byteAt: 1) bitAnd: 1) = 0
]

{ #category : #'truncation and round off' }
Integer >> floor [ 
	"Refer to the comment in Number|floor."
]

{ #category : #'mathematical functions' }
Integer >> gcd: anInteger [
	"See Knuth, Vol 2, 4.5.2, Algorithm L"
	"Initialize"
	| higher u v k uHat vHat a b c d vPrime vPrimePrime q t |
	higher := SmallInteger maxVal highBit.
	u := self abs max: (v := anInteger abs).
	v := self abs min: v.
	[v class == SmallInteger]
		whileFalse: 
			[(uHat := u bitShift: (k := higher - u highBit)) class == SmallInteger
				ifFalse: 
					[k := k - 1.
					uHat := uHat bitShift: -1].
			vHat := v bitShift: k.
			a := 1.
			b := 0.
			c := 0.
			d := 1.
			"Test quotient"
			[(vPrime := vHat + d) ~= 0
				and: [(vPrimePrime := vHat + c) ~= 0 and: [(q := uHat + a // vPrimePrime) = (uHat + b // vPrime)]]]
				whileTrue: 
					["Emulate Euclid"
					c := a - (q * (a := c)).
					d := b - (q * (b := d)).
					vHat := uHat - (q * (uHat := vHat))].
			"Multiprecision step"
			b = 0
				ifTrue: 
					[v := u rem: (u := v)]
				ifFalse: 
					[t := u * a + (v * b).
					v := u * c + (v * d).
					u := t]].
	^ v gcd: u
]

{ #category : #private }
Integer >> growby: n [

	^self growto: self bytesCount + n
]

{ #category : #private }
Integer >> growto: n [

	^self copyto: (self species new: n)
]

{ #category : #converting }
Integer >> hex [
	"Returns a string representation of the receiver as hex, prefixed with 16r.  
	DO NOT CHANGE THIS!  The Cog VMMaker depends on this."
	
	"Consider using any of
		printStringHex
		printStringBase: 16
		printStringBase: 16 length: 8 padded: true
		storeStringHex
		storeStringBase: 16
		storeStringBase: 16 length: 11 padded: true"
		
	"12 hex >>> '16r2'"	
	"10 hex >>> '16rA'"	
	"-10 hex >>> '-16rA'"	
		
	^ self storeStringBase: 16
]

{ #category : #'bit manipulation' }
Integer >> highBit [
	"Answer the index of the high order bit of the receiver, or zero if the  
	receiver is zero. Raise an error if the receiver is negative, since  
	negative integers are defined to have an infinite number of leading 1's 
	in 2's-complement arithmetic. Use >>highBitOfMagnitude if you want to  
	get the highest bit of the magnitude."

	^ self subclassResponsibility
]

{ #category : #'bit manipulation' }
Integer >> highBitOfMagnitude [
	"Answer the index of the high order bit of the magnitude of the  
	receiver, or zero if the receiver is zero."
	^ self subclassResponsibility
]

{ #category : #testing }
Integer >> isFraction [
	"Each Integer is considered as a special kind of Fraction with self as numerator and a unit denominator.
	Rationale: A Fraction with a unit denominator will be automatically reduced to an Integer.
	Hence Integer has to be polymorphic to Fraction."
	^true
]

{ #category : #testing }
Integer >> isInteger [
	"True for all subclasses of Integer."

	^ true
]

{ #category : #printing }
Integer >> isLiteral [

	^true
]

{ #category : #testing }
Integer >> isPowerOfTwo [
	"Return true if the receiver is an integral power of two."
	^ self ~= 0 and: [(self bitAnd: self-1) = 0]
]

{ #category : #'system primitives' }
Integer >> lastDigit [
	"Answer the last digit of the integer base 256.  LargePositiveInteger uses bytes of base two number, and each is a 'digit'."

	^self byteAt: self bytesCount
]

{ #category : #'bit manipulation' }
Integer >> lowBit [
	"Answer the index of the low order bit of this number."
	| index |
	self = 0 ifTrue: [ ^ 0 ].
	index := 1.
	[ (self byteAt: index) = 0 ]
		whileTrue:
			[ index := index + 1 ].
	^ (self byteAt: index) lowBit + (8 * (index - 1))
]

{ #category : #'bit manipulation' }
Integer >> noMask: mask [ 
	"Treat the argument as a bit mask. Answer whether none of the bits that 
	are 1 in the argument are 1 in the receiver."

	^0 = (self bitAnd: mask)
]

{ #category : #'truncation and round off' }
Integer >> normalize [ 
	"SmallInts OK; LgInts override"
	^ self
]

{ #category : #printing }
Integer >> numberOfDigits [
	"Return how many digits are necessary to print this number in base 10.
	This does not count any place for minus sign, radix prefix or whatever."

	^ self numberOfDigitsInBase: 10 

]

{ #category : #printing }
Integer >> numberOfDigitsInBase: b [ 
	"Return how many digits are necessary to print this number in base b.
	This does not count any place for minus sign, radix prefix or whatever.
	Note that this algorithm may cost a few operations on LargeInteger."

	| nDigits q total |
	self negative ifTrue: [^self negated numberOfDigitsInBase: b].
	self < b ifTrue: [^1].
	b isPowerOfTwo ifTrue: [^self highBit + b highBit - 2 quo: b highBit - 1].
	
	"A conversion from base 2 to base b has to be performed.
	This algorithm avoids Float computations like (self log: b) floor + 1,
	1) because they are inexact
	2) because LargeInteger might overflow
	3) because this algorithm might be cheaper than conversion"

	q := self.
	total := 0.
	["Make an initial nDigits guess that is lower than or equal to required number of digits"
	nDigits := b = 10
		ifTrue: [((q highBit - 1) * 1233 >> 12) + 1. "This is because (2 log)/(10 log)*4096 is slightly greater than 1233"]
		ifFalse: [q highBit quo: b highBit].
	total := total + nDigits.
	
	"See how many digits remains above these first nDigits guess"
	(q := q quo: (b raisedToInteger: nDigits)) < b] whileFalse.
	^q = 0
		ifTrue: [total]
		ifFalse: [total + 1]
]

{ #category : #accessing }
Integer >> numerator [
	"Let an Integer be polymorphic to a Fraction. See #isFraction."
	^self
]

{ #category : #printing }
Integer >> print: positiveNumberString on: aStream prefix: prefix length: minimum padded: zeroFlag [
	"Return a String based on concatenation of positiveNumberString with prefix then padded by 0 is zeroFlag is set for a minimum length."
	
	"(String streamContents: [:s | 10 print: '10' on: s prefix: 'zz' length: 4 padded: true] ) >>> 'zz10'"
	

	| padLength |
	padLength := minimum - positiveNumberString size - prefix size.
	padLength > 0
		ifTrue: [zeroFlag
						ifTrue: [aStream nextPutAll: prefix; nextPutAll: (String new: padLength withAll: $0)]
						ifFalse: [aStream nextPutAll: (String new: padLength withAll: Character space); nextPutAll: prefix]]
		ifFalse: [aStream nextPutAll: prefix].
	aStream nextPutAll: positiveNumberString
	
]

{ #category : #'printing-numerative' }
Integer >> printHexByteOn: characterWriteStream [
	"Assuming the receiver is a byte value [0, 255],
	print a 2 character hexadecimal representation on characterWriteStream
	using the uppercase alphabet."
	
	(self between: 0 and: 255)
		ifTrue: [
			| highNibble lowNibble hexDigits |
			highNibble := self bitShift: -4.
			lowNibble := self bitAnd: 15.
			hexDigits := '0123456789ABCDEF'.
			characterWriteStream 
				nextPut: (hexDigits at: highNibble + 1);
				nextPut: (hexDigits at: lowNibble + 1) ]
		ifFalse: [ 
			self error: 'byte value between 0 and 255 expected' ]
]

{ #category : #'printing-numerative' }
Integer >> printLowercaseHexByteOn: characterWriteStream [
	"Assuming the receiver is a byte value [0, 255],
	print a 2 character hexadecimal representation on characterWriteStream
	using the lowercase alphabet."
	
	(self between: 0 and: 255)
		ifTrue: [
			| highNibble lowNibble hexDigits |
			highNibble := self bitShift: -4.
			lowNibble := self bitAnd: 15.
			hexDigits := '0123456789abcdef'.
			characterWriteStream 
				nextPut: (hexDigits at: highNibble + 1);
				nextPut: (hexDigits at: lowNibble + 1) ]
		ifFalse: [ 
			self error: 'byte value between 0 and 255 expected' ]
]

{ #category : #'printing-numerative' }
Integer >> printOn: aStream base: base length: minimum padded: zeroFlag [
	"Return a String representation of this number in base b with a minimum length and padded by 0 if zeroFlag is set"
	
	"(String streamContents: [ :s | 10 printOn: s base: 10 length: 4 padded: true] ) >>> '0010'"
	"(String streamContents: [ :s | -10 printOn: s base: 10 length: 4 padded: true] ) >>> '-010'"
	

	| prefix |	
	prefix := self negative ifTrue: ['-'] ifFalse: [ '' ].
	self 
		print: (self abs printStringBase: base) 
		on: aStream 
		prefix: prefix 
		length: minimum 
		padded: zeroFlag

]

{ #category : #'printing-numerative' }
Integer >> printOn: aStream base: b nDigits: n [ 
	"Append a representation of this number in base b on aStream using nDigits.
	self must be positive."

	self subclassResponsibility
]

{ #category : #printing }
Integer >> printOn: outputStream base: baseInteger showRadix: flagBoolean [ 
	"Write a sequence of characters that describes the receiver in radix 
	baseInteger with optional radix specifier. 
	The result is undefined if baseInteger less than 2 or greater than 36."
	
	| tempString startPos |

	tempString := self printStringRadix: baseInteger.
	flagBoolean 
		ifTrue: [ ^ outputStream nextPutAll: tempString ].
		
	startPos := (tempString indexOf: $r ifAbsent: [ self error: 'radix indicator not found.' ]) + 1.
	self negative ifTrue: [ outputStream nextPut: $-] .
	outputStream nextPutAll: (tempString copyFrom: startPos to: tempString size)
]

{ #category : #printing }
Integer >> printOn: aStream showingDecimalPlaces: placesDesired [
	"Same as super, but provides a faster implementation because fraction part and rounding are trivial."
	
	self printOn: aStream base: 10.
	
	placesDesired <= 0 ifTrue: [ ^ self ].
	
	aStream nextPut: $..
	placesDesired timesRepeat: [ aStream nextPut: $0 ]
]

{ #category : #printing }
Integer >> printPaddedWith: aCharacter to: anInteger [ 
	"Answer the string containing the ASCII representation of the receiver 
	padded on the left with aCharacter to be at least anInteger characters."
	
	^ self
		printPaddedWith: aCharacter
		to: anInteger
		base: 10
]

{ #category : #printing }
Integer >> printPaddedWith: aCharacter to: anInteger base: aRadix [ 
	"Answer the string containing the ASCII representation of the receiver 
	padded on the left with aCharacter to be at least anInteger characters."
	| aStream padding digits |
	
	aStream := (String new: 10) writeStream.
	self
		printOn: aStream
		base: aRadix
		showRadix: false.
	digits := aStream contents.
	padding := anInteger - digits size.
	padding > 0 ifFalse: [^ digits].
	^ ((String new: padding) atAllPut: aCharacter;
	 yourself) , digits
]

{ #category : #printing }
Integer >> printSeparatedBy: aDelimiter every: offset signed: printSigned base: base on: aStream [
	| digits |
	digits := self abs printStringBase: base.
	
	self signBit = 1 
		ifTrue: [ aStream nextPut: $- ] 
		ifFalse: [ printSigned ifTrue: [ aStream nextPut: $+ ] ].
	
	1 to: digits size do: [ :i | 
		aStream nextPut: (digits at: i).
		(i < digits size and: [ (i - digits size) \\ offset = 0 ])
			ifTrue: [ aStream nextPut: aDelimiter ] ]
]

{ #category : #printing }
Integer >> printSeparatedBy: aDelimiter every: offset signed: printSigned on: aStream [
	^ self printSeparatedBy: aDelimiter every: offset signed: printSigned base: 10 on: aStream
]

{ #category : #printing }
Integer >> printString [
	"For Integer, prefer the stream version to the string version for efficiency"
	
	^String streamContents: [:str | self printOn: str base: 10]
]

{ #category : #'printing-numerative' }
Integer >> printStringBase: base length: minimum padded: zeroFlag [

	^ String streamContents: [:s| self printOn: s base: base length: minimum padded: zeroFlag]
]

{ #category : #'printing-numerative' }
Integer >> printStringHex [
	"Returns the hex digit part of the integer when printed in hexadecimal format."
	"30 printStringHex >>> '1E'"
	"30 hex >>> '16r1E'"
		
	^ self printStringBase: 16
]

{ #category : #'printing-numerative' }
Integer >> printStringLength: minimal [
	^self printStringLength: minimal padded: false

]

{ #category : #'printing-numerative' }
Integer >> printStringLength: minimal padded: zeroFlag [
	^self printStringBase: 10 length: minimal padded: zeroFlag
]

{ #category : #'printing-numerative' }
Integer >> printStringPadded: minimal [
	^self printStringLength: minimal padded: true

]

{ #category : #printing }
Integer >> printStringRadix: baseInteger [ 
	"Return a string containing a sequence of characters that represents the 
	numeric value of the receiver in the radix specified by the argument.  
	If the receiver is negative, a minus sign ('-') is prepended to the 
	sequence of characters. 
	The result is undefined if baseInteger less than 2 or greater than 36."
	
	| tempString |
	
	baseInteger = 10
		ifTrue: [
			tempString := self storeStringBase: baseInteger.
			self negative
				ifTrue: [^ '-10r' , (tempString copyFrom: 2 to: tempString size)]
				ifFalse: [^ '10r' , tempString]].
			
	^ self storeStringBase: baseInteger
]

{ #category : #printing }
Integer >> printWithCommasOn: aStream [
	"123456789 asStringWithCommas"
	"-123456789 asStringWithCommas"
	^ self printSeparatedBy: $, every: 3 signed: false on: aStream
]

{ #category : #printing }
Integer >> printWithCommasSignedOn: aStream [
	"123456789 asStringWithCommasSigned"
	"-123456789 asStringWithCommasSigned"
	^ self printSeparatedBy: $, every: 3 signed: true on: aStream
]

{ #category : #arithmetic }
Integer >> quo: aNumber [ 
	"Refer to the comment in Number quo: "
	| ng quo |
	aNumber isInteger ifTrue: 
		[ng := self negative == aNumber negative == false.
		quo := (self digitDiv: aNumber neg: ng) at: 1.
		^ quo normalize].
	^ aNumber adaptToInteger: self andSend: #quo:
]

{ #category : #'printing-numerative' }
Integer >> radix: base [ 
	^ self printStringBase: base
]

{ #category : #arithmetic }
Integer >> reciprocalModulo: n [
	"Answer an integer x such that (self * x) \\ n = 1, x > 0, x < n.
	Raise an error if there is no such integer.
	The algorithm is a non extended euclidean modular inversion called NINV.
	It is described in this article:
		'Using an RSA Accelerator for Modular Inversion'
	by Martin Seysen. See http://www.iacr.org/archive/ches2005/017.pdf"

	| u v f fPlusN b result result2 |
	((self <= 0) or: [n <= 0]) ifTrue: [self error: 'self and n must be greater than zero'].
	self >= n ifTrue: [self error: 'self must be < n'].

	b := n highBit + 1.
	f := 1 bitShift: b.
	v := (self bitShift: b) + 1.
	u := n bitShift: b.
	fPlusN := f + n.
	[v >= fPlusN] whileTrue:
		[v := u \\ (u := v)].
	result := v - f.
	(result2 := result + n) > 0
		ifFalse: [self error: 'no inverse'].
	^result positive
		ifTrue: [result]
		ifFalse: [result2]
]

{ #category : #'system primitives' }
Integer >> replaceFrom: start to: stop with: replacement startingAt: repStart [
	| j |  "Catches failure if LgInt replace primitive fails"
	j := repStart.
	start to: stop do:
		[:i |
		self byteAt: i put: (replacement byteAt: j).
		j := j+1]
]

{ #category : #'truncation and round off' }
Integer >> round: numberOfWishedDecimal [
	"Round the decimal part of the receiver to be limited to the number of wished decimal. Only leave a fixed amount of decimal. Integers there are already rounded."
 
	^self
]

{ #category : #'truncation and round off' }
Integer >> rounded [ 
	"Refer to the comment in Number|rounded."
]

{ #category : #printing }
Integer >> storeOn: aStream [
	self < 0
		ifTrue: [ aStream space ].
		
	super storeOn: aStream.
]

{ #category : #'printing-numerative' }
Integer >> storeOn: aStream base: base [
	"Print a representation of the receiver on the stream
	<aStream> in base <base> where
	2 <= <baseInteger> <= 16. If <base> is other than 10
	it is written first separated by $r followed by the number
	like for example: 16rFCE2"

	| integer |
	integer := self negative
		ifTrue: [aStream nextPut: $-. self negated]
		ifFalse: [self].
	base = 10 ifFalse: [aStream nextPutAll: base printString; nextPut: $r].
	aStream nextPutAll: (integer printStringBase: base).

]

{ #category : #'printing-numerative' }
Integer >> storeOn: aStream base: base length: minimum padded: zeroFlag [
	| prefix |
	prefix := self negative ifTrue: ['-'] ifFalse: [String new].
	base = 10 ifFalse: [prefix := prefix, base printString, 'r'].
	self print: (self abs printStringBase: base) on: aStream prefix: prefix length: minimum padded: zeroFlag

]

{ #category : #'printing-numerative' }
Integer >> storeStringBase: base length: minimum padded: zeroFlag [
	"(10 storeStringBase: 16 length: 6 padded: true) >>> '16r00A'"

	^ String streamContents: [:s| 
			self storeOn: s base: base length: minimum padded: zeroFlag ]
]

{ #category : #'printing-numerative' }
Integer >> storeStringHex [
	"Returns a string representation of the receiver in base 16 in a way that executing it will return the receiver."
	"10 storeStringHex >>> '16rA'"

	^ self storeStringBase: 16
]

{ #category : #enumerating }
Integer >> timesRepeat: aBlock [ 
	"Evaluate the argument, aBlock, the number of times represented by the 
	receiver."

	| count |
	count := 1.
	[count <= self]
		whileTrue: 
			[aBlock value.
			count := count + 1]
]

{ #category : #'truncation and round off' }
Integer >> truncated [ 
	"Refer to the comment in Number|truncated."
]
