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
	#category : #Kernel-Numbers
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
	value digitAt: 4 put: byte4.
	value digitAt: 3 put: byte3.
	value digitAt: 2 put: byte2.
	value digitAt: 1 put: byte1.
	^ value
]

{ #category : #'prime numbers' }
Integer class >> largePrimesUpTo: maxValue [
	"Compute and return all the prime numbers up to maxValue"
	^Array streamContents:[:s| self largePrimesUpTo: maxValue do:[:prime| s nextPut: prime]]
]

{ #category : #'prime numbers' }
Integer class >> largePrimesUpTo: max do: aBlock [
	"Evaluate aBlock with all primes up to maxValue.
	The Algorithm is adapted from http://www.rsok.com/~jrm/printprimes.html
	It encodes prime numbers much more compactly than #primesUpTo: 
	38.5 integer per byte (2310 numbers per 60 byte) allow for some fun large primes.
	(all primes up to SmallInteger maxVal can be computed within ~27MB of memory;
	the regular #primesUpTo: would require 4 *GIGA*bytes).
	Note: The algorithm could be re-written to produce the first primes (which require
	the longest time to sieve) faster but only at the cost of clarity."

	| limit flags maskBitIndex bitIndex maskBit byteIndex index primesUpTo2310 indexLimit |
	limit := max asInteger - 1.
	indexLimit := max sqrt truncated + 1.
	"Create the array of flags."
	flags := ByteArray new: (limit + 2309) // 2310 * 60 + 60.
	flags atAllPut: 16rFF. "set all to true"

	"Compute the primes up to 2310"
	primesUpTo2310 := self primesUpTo: 2310.

	"Create a mapping from 2310 integers to 480 bits (60 byte)"
	maskBitIndex := Array new: 2310.
	bitIndex := -1. "for pre-increment"
	maskBitIndex at: 1 put: (bitIndex := bitIndex + 1).
	maskBitIndex at: 2 put: (bitIndex := bitIndex + 1).

	1 to: 5 do:[:i| aBlock value: (primesUpTo2310 at: i)].

	index := 6.
	2 to: 2309 do:[:n|
		[(primesUpTo2310 at: index) < n] 
			whileTrue:[index := index + 1].
		n = (primesUpTo2310 at: index) ifTrue:[
			maskBitIndex at: n+1 put: (bitIndex := bitIndex + 1).
		] ifFalse:[
			"if modulo any of the prime factors of 2310, then could not be prime"
			(n \\ 2 = 0 or:[n \\ 3 = 0 or:[n \\ 5 = 0 or:[n \\ 7 = 0 or:[n \\ 11 = 0]]]]) 
				ifTrue:[maskBitIndex at: n+1 put: 0]
				ifFalse:[maskBitIndex at: n+1 put: (bitIndex := bitIndex + 1)].
		].
	].

	"Now the real work begins...
	Start with 13 since multiples of 2,3,5,7,11 are handled by the storage method;
	increment by 2 for odd numbers only."
	13 to: limit by: 2 do:[:n|
		(maskBit := maskBitIndex at: (n \\ 2310 + 1)) = 0 ifFalse:["not a multiple of 2,3,5,7,11"
			byteIndex := n // 2310 * 60 + (maskBit-1 bitShift: -3) + 1.
			bitIndex := 1 bitShift: (maskBit bitAnd: 7).
			((flags at: byteIndex) bitAnd: bitIndex) = 0 ifFalse:["not marked -- n is prime"
				aBlock value: n.
				"Start with n*n since any integer < n has already been sieved 
				(e.g., any multiple of n with a number k < n has been cleared 
				when k was sieved); add 2 * i to avoid even numbers and
				mark all multiples of this prime. Note: n < indexLimit below
				limits running into LargeInts -- nothing more."
				n < indexLimit ifTrue:[
					index := n * n.
					(index bitAnd: 1) = 0 ifTrue:[index := index + n].
					[index <= limit] whileTrue:[
						(maskBit := maskBitIndex at: (index \\ 2310 + 1)) = 0 ifFalse:[
							byteIndex := (index // 2310 * 60) + (maskBit-1 bitShift: -3) + 1.
							maskBit := 255 - (1 bitShift: (maskBit bitAnd: 7)).
							flags at: byteIndex put: ((flags at: byteIndex) bitAnd: maskBit).
						].
						index := index + (2 * n)].
				].
			].
		].
	].

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

{ #category : #'prime numbers' }
Integer class >> primesUpTo: max [
	"Return a list of prime integers up to the given integer."
	"Integer primesUpTo: 100"
	^Array streamContents:[:s| self primesUpTo: max do:[:prime| s nextPut: prime]]
]

{ #category : #'prime numbers' }
Integer class >> primesUpTo: max do: aBlock [
	"Compute aBlock with all prime integers up to the given integer."
	"Integer primesUpTo: 100"

	| limit flags prime k |
	limit := max asInteger - 1.
	"Fall back into #largePrimesUpTo:do: if we'd require more than 100k of memory; 
	the alternative will only requre 1/154th of the amount we need here and is almost as fast."
	limit > 25000 ifTrue:[^self largePrimesUpTo: max do: aBlock].
	flags := (Array new: limit) atAllPut: true.
	1 to: limit - 1 do: [:i |
		(flags at: i) ifTrue: [
			prime := i + 1.
			k := i + prime.
			[k <= limit] whileTrue: [
				flags at: k put: false.
				k := k + prime].
			aBlock value: prime]].

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
						ifTrue: [^ (self digitCompare: aNumber) > 0]
						ifFalse: [^ (self digitCompare: aNumber) < 0]]
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
						ifTrue: [^ (self digitCompare: aNumber) >= 0]
						ifFalse: [^ (self digitCompare: aNumber) <= 0]]
			ifFalse: [^ self negative]].
	^ aNumber adaptToInteger: self andCompare: #<=
]

{ #category : #comparing }
Integer >> = aNumber [
	aNumber isNumber ifFalse: [^ false].
	aNumber isInteger ifTrue:
		[aNumber negative == self negative
			ifTrue: [^ (self digitCompare: aNumber) = 0]
			ifFalse: [^ false]].
	^ aNumber adaptToInteger: self andCompare: #=
]

{ #category : #comparing }
Integer >> > aNumber [
	aNumber isInteger ifTrue:
		[self negative == aNumber negative
			ifTrue: [self negative
						ifTrue: [^(self digitCompare: aNumber) < 0]
						ifFalse: [^(self digitCompare: aNumber) > 0]]
			ifFalse: [^ aNumber negative]].
	^ aNumber adaptToInteger: self andCompare: #>
]

{ #category : #comparing }
Integer >> >= aNumber [
	aNumber isInteger ifTrue:
		[self negative == aNumber negative
			ifTrue: [self negative
						ifTrue: [^(self digitCompare: aNumber) <= 0]
						ifFalse: [^(self digitCompare: aNumber) >= 0]]
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
			digit := magnitude digitAt: firstDigitIx.
			^ (digit bitAnd: mask)
				~= 0].
	((magnitude digitAt: firstDigitIx)
			bitShift: rightShift)
			~= 0
		ifTrue: [^ true].
	firstDigitIx + 1
		to: lastDigitIx - 1
		do: [:ix | (magnitude digitAt: ix)
					~= 0
				ifTrue: [^ true]].
	(((magnitude digitAt: lastDigitIx)
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

{ #category : #converting-arrays }
Integer >> asArray [

	| stream |
	stream := Array new writeStream.
	self digitLength to: 1 by: -1 do: [:digitIndex |
		stream nextPut: (self digitAt: digitIndex)].
	^ stream contents

]

{ #category : #converting-arrays }
Integer >> asByteArray [

	| stream |
	stream := ByteArray new writeStream.
	self digitLength to: 1 by: -1 do: [:digitIndex |
		stream nextPut: (self digitAt: digitIndex)].
	^ stream contents

]

{ #category : #converting-arrays }
Integer >> asByteArrayOfSize: aSize [ 
	"Answer a ByteArray of aSize with my value, most-significant byte first."
	| answer digitPos |
	aSize < self digitLength ifTrue: [ self error: 'number to large for byte array' ].
	answer := ByteArray new: aSize.
	digitPos := 1.
	aSize 
		to: aSize - self digitLength + 1
		by: -1
		do: 
			[ :pos | 
			answer 
				at: pos
				put: (self digitAt: digitPos).
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

{ #category : #printing }
Integer >> asWords [
	"SmallInteger maxVal asWords"
	| mils minus three num answer milCount |
	self = 0 ifTrue: [^'zero'].
	mils := #('' ' thousand' ' million' ' billion' ' trillion' ' quadrillion' ' quintillion' ' sextillion' ' septillion' ' octillion' ' nonillion' ' decillion' ' undecillion' ' duodecillion' ' tredecillion' ' quattuordecillion' ' quindecillion' ' sexdecillion' ' septendecillion' ' octodecillion' ' novemdecillion' ' vigintillion').
	num := self.
	minus := ''.
	self < 0 ifTrue: [
		minus := 'negative '.
		num := num negated.
	].
	answer := String new.
	milCount := 1.
	[num > 0] whileTrue: [
		three := (num \\ 1000) threeDigitName.
		num := num // 1000.
		three isEmpty ifFalse: [
			answer isEmpty ifFalse: [
				answer := ', ',answer
			].
			answer := three,(mils at: milCount),answer.
		].
		milCount := milCount + 1.
	].
	^minus,answer
]

{ #category : #converting }
Integer >> asYear [

 	^ Year year: self  
]

{ #category : #benchmarks }
Integer >> benchFib [  "Handy send-heavy benchmark"
	"(result // seconds to run) = approx calls per second"
	" | r t |
	  t := Time millisecondsToRun: [r := 26 benchFib].
	  (r * 1000) // t"
	"138000 on a Mac 8100/100"
	^ self < 2
		ifTrue: [1] 
		ifFalse: [(self-1) benchFib + (self-2) benchFib + 1]

]

{ #category : #benchmarks }
Integer >> benchmark [  "Handy bytecode-heavy benchmark"
	"(500000 // time to run) = approx bytecodes per second"
	"5000000 // (Time millisecondsToRun: [10 benchmark]) * 1000"
	"3059000 on a Mac 8100/100"
    | size flags prime k count |
    size := 8190.
    1 to: self do:
        [:iter |
        count := 0.
        flags := (Array new: size) atAllPut: true.
        1 to: size do:
            [:i | (flags at: i) ifTrue:
                [prime := i+1.
                k := i + prime.
                [k <= size] whileTrue:
                    [flags at: k put: false.
                    k := k + prime].
                count := count + 1]]].
    ^ count
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
		length: (self digitLength max: norm digitLength)
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
		length: (self digitLength max: norm digitLength)
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
		lookfirst: self digitLength) normalize
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

      ^self digitLength * 8
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
		length: (self digitLength max: norm digitLength)
]

{ #category : #'truncation and round off' }
Integer >> ceiling [ 
	"Refer to the comment in Number|ceiling."
]

{ #category : #private }
Integer >> copyto: x [
	| stop |
	stop := self digitLength min: x digitLength.
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
Integer >> decimalDigitAt: anExponent [

	"Return number that represents digit at given decimal position.
		42 digitAt: 2 base: 10 -> 4
		42 digitAt: 1 base: 10 -> 1
	It is always a number or zero:
		1 digitAt: 2 base: 10 -> 0
	Results are not defined non-integer arguments."

	^ self digitAt: anExponent base: 10
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
	(len := self digitLength) < (arglen := arg digitLength) ifTrue: [len := arglen].
	"Open code max: for speed"
	sum := Integer new: len neg: self negative.
	1 to: len do: 
		[:i | 
		accum := (accum bitShift: -8)
					+ (self digitAt: i) + (arg digitAt: i).
		sum digitAt: i put: (accum bitAnd: 255)].
	accum > 255
		ifTrue: 
			[sum := sum growby: 1.
			sum at: sum digitLength put: (accum bitShift: -8)].
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
Integer >> digitCompare: arg [ 
	"Compare the magnitude of self with that of arg.   
	Return a code of 1, 0, -1 for self >, = , < arg"
	| len arglen argDigit selfDigit |
	<primitive: 'primDigitCompare' module:'LargeIntegers'>
	len := self digitLength.
	(arglen := arg digitLength) ~= len
		ifTrue: [arglen > len
				ifTrue: [^ -1]
				ifFalse: [^ 1]].
	[len > 0]
		whileTrue: 
			[(argDigit := arg digitAt: len) ~= (selfDigit := self digitAt: len)
				ifTrue: [argDigit < selfDigit
						ifTrue: [^ 1]
						ifFalse: [^ -1]].
			len := len - 1].
	^ 0
]

{ #category : #private }
Integer >> digitDiv: arg neg: ng [ 
	"Answer with an array of (quotient, remainder)."
	| quo rem ql d div dh dnh dl qhi qlo j l hi lo r3 a t |
	<primitive: 'primDigitDivNegative' module:'LargeIntegers'>
	arg = 0 ifTrue: [^ (ZeroDivide dividend: self) signal].
	"TFEI added this line"
	l := self digitLength - arg digitLength + 1.
	l <= 0 ifTrue: [^ Array with: 0 with: self].
	"shortcut against #highBit"
	d := 8 - arg lastDigit highBitOfPositiveReceiver.
	div := arg digitLshift: d.
	div := div growto: div digitLength + 1.
	"shifts so high order word is >=128"
	rem := self digitLshift: d.
	rem digitLength = self digitLength ifTrue: [rem := rem growto: self digitLength + 1].
	"makes a copy and shifts"
	quo := Integer new: l neg: ng.
	dl := div digitLength - 1.
	"Last actual byte of data"
	ql := l.
	dh := div digitAt: dl.
	dnh := dl = 1
				ifTrue: [0]
				ifFalse: [div digitAt: dl - 1].
	1 to: ql do: 
		[:k | 
		"maintain quo*arg+rem=self"
		"Estimate rem/div by dividing the leading to bytes of rem by dh."
		"The estimate is q = qhi*16+qlo, where qhi and qlo are nibbles."
		j := rem digitLength + 1 - k.
		"r1 := rem digitAt: j."
		(rem digitAt: j)
			= dh
			ifTrue: [qhi := qlo := 15
				"i.e. q=255"]
			ifFalse: 
				["Compute q = (r1,r2)//dh, t = (r1,r2)\\dh.  
				Note that r1,r2 are bytes, not nibbles.  
				Be careful not to generate intermediate results exceeding 13  
				bits."
				"r2 := (rem digitAt: j - 1)."
				t := ((rem digitAt: j)
							bitShift: 4)
							+ ((rem digitAt: j - 1)
									bitShift: -4).
				qhi := t // dh.
				t := (t \\ dh bitShift: 4)
							+ ((rem digitAt: j - 1)
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
							ifFalse: [rem digitAt: j - 2].
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
		1 to: div digitLength do: 
			[:i | 
			hi := (div digitAt: i)
						* qhi.
			lo := a + (rem digitAt: l) - ((hi bitAnd: 15)
							bitShift: 4) - ((div digitAt: i)
							* qlo).
			rem digitAt: l put: lo - (lo // 256 * 256).
			"sign-tolerant form of (lo bitAnd: 255)"
			a := lo // 256 - (hi bitShift: -4).
			l := l + 1].
		a < 0
			ifTrue: 
				["Add div back into rem, decrease q by 1"
				qlo := qlo - 1.
				l := j - dl.
				a := 0.
				1 to: div digitLength do: 
					[:i | 
					a := (a bitShift: -8)
								+ (rem digitAt: l) + (div digitAt: i).
					rem digitAt: l put: (a bitAnd: 255).
					l := l + 1]].
		quo digitAt: quo digitLength + 1 - k put: (qhi bitShift: 4)
				+ qlo].
	rem := rem
				digitRshift: d
				bytes: 0
				lookfirst: dl.
	^ Array with: quo with: rem
]

{ #category : #'system primitives' }
Integer >> digitLength [
	self subclassResponsibility
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
		whileTrue: [b1 := self digitAt: i.
			neg1
				ifTrue: [b1 := z1
								ifTrue: [b1 = 0
										ifTrue: [0]
										ifFalse: [z1 := false.
											256 - b1]]
								ifFalse: [255 - b1]].
			b2 := arg digitAt: i.
			neg2
				ifTrue: [b2 := z2
								ifTrue: [b2 = 0
										ifTrue: [0]
										ifFalse: [z2 := false.
											256 - b2]]
								ifFalse: [255 - b2]].
			b := b1 perform: op with: b2.
			result
				digitAt: i
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
	1 to: byteShift do: [:i | result digitAt: i put: 0].
	1 to: len - byteShift do: 
		[:i | 
		digit := self digitAt: i.
		result digitAt: i + byteShift put: (((digit bitAnd: mask)
				bitShift: bitShift)
				bitOr: carry).
		carry := digit bitShift: rShift].
	^ result
]

{ #category : #private }
Integer >> digitMultiply: arg neg: ng [ 
	| prod prodLen carry digit k ab |
	<primitive: 'primDigitMultiplyNegative' module:'LargeIntegers'>
	(arg digitLength = 1 and: [(arg digitAt: 1)
			= 0])
		ifTrue: [^ 0].
	(self digitLength = 1 and: [(self digitAt: 1)
			= 0])
		ifTrue: [^ 0].
	prodLen := self digitLength + arg digitLength.
	prod := Integer new: prodLen neg: ng.
	"prod starts out all zero"
	1 to: self digitLength do: [:i | (digit := self digitAt: i) ~= 0
			ifTrue: 
				[k := i.
				carry := 0.
				"Loop invariant: 0<=carry<=0377, k=i+j-1"
				1 to: arg digitLength do: 
					[:j | 
					ab := (arg digitAt: j)
								* digit + carry + (prod digitAt: k).
					carry := ab bitShift: -8.
					prod digitAt: k put: (ab bitAnd: 255).
					k := k + 1].
				prod digitAt: k put: carry]].
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
	digit := self digitAt: i.
	[((digit bitShift: n) bitOr: x) = 0 and: [i ~= 1]] whileTrue:
		[x := digit bitShift: f "Can't exceed 8 bits".
		i := i - 1.
		digit := self digitAt: i].
	i <= b ifTrue: [^Integer new: 0 neg: self negative].  "All bits lost"
	r := Integer new: i - b neg: self negative.
	count := i.
	x := (self digitAt: b + 1) bitShift: n.
	b + 1 to: count do:
		[:j | digit := self digitAt: j + 1.
		r digitAt: j - b put: (((digit bitAnd: m) bitShift: f) bitOr: x) 
			"Avoid values > 8 bits".
		x := digit bitShift: n].
	^r
]

{ #category : #private }
Integer >> digitSubtract: arg [ 
	| smaller larger z sum sl al ng |
	<primitive: 'primDigitSubtract' module:'LargeIntegers'>
	sl := self digitLength.
	al := arg digitLength.
	(sl = al
		ifTrue: 
			[[(self digitAt: sl)
				= (arg digitAt: sl) and: [sl > 1]]
				whileTrue: [sl := sl - 1].
			al := sl.
			(self digitAt: sl)
				< (arg digitAt: sl)]
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
		z := z + (larger digitAt: i) - (smaller digitAt: i).
		sum digitAt: i put: z - (z // 256 * 256).
		"sign-tolerant form of (z bitAnd: 255)"
		z := z // 256].
	^ sum normalize
]

{ #category : #testing }
Integer >> even [ 
	"Refer to the comment in Number|even."

	^((self digitAt: 1) bitAnd: 1) = 0
]

{ #category : #'mathematical functions' }
Integer >> factorial [
	"Answer the factorial of the receiver."

	self = 0 ifTrue: [^ 1].
	self > 0 ifTrue: [^ self * (self - 1) factorial].
	self error: 'Not valid for negative integers'
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

	^self growto: self digitLength + n
]

{ #category : #private }
Integer >> growto: n [

	^self copyto: (self species new: n)
]

{ #category : #converting }
Integer >> hex [
	"Print the receiver as hex, prefixed with 16r.  DO NOT CHANGE THIS!  The Cog VMMaker depends on this.
	 Consider using any of
		printStringHex
		printStringBase: 16
		printStringBase: 16 length: 8 padded: true
		storeStringHex
		storeStringBase: 16
		storeStringBase: 16 length: 11 padded: true"
	^self storeStringBase: 16
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
Integer >> isMostLikelyPrime [
	"See isProbablyPrimeWithK:andQ: for the algoritm description."
	| k q |
	self <= 1 ifTrue: [^self error: 'operation undefined'].
	self even ifTrue: [^self = 2].
	k := 1.

	q := self - 1 bitShift: -1.
	[q odd] whileFalse: 
			[q := q bitShift: -1.
			k := k + 1].
	
	25 timesRepeat: [(self isProbablyPrimeWithK: k andQ: q) ifFalse: [^false]].
	^true
]

{ #category : #testing }
Integer >> isPowerOfTwo [
	"Return true if the receiver is an integral power of two."
	^ self ~= 0 and: [(self bitAnd: self-1) = 0]
]

{ #category : #testing }
Integer >> isPrime [
	"Answer true if the receiver is a prime number. See isProbablyPrime for a probabilistic
	implementation that is much faster for large integers, and that is correct to an extremely
	high statistical level of confidence (effectively deterministic)."
	
	self <= 1 ifTrue: [ ^false ].
	self even ifTrue: [ ^self = 2].
	3 to: self sqrtFloor by: 2 do: [ :each |
		self \\ each = 0 ifTrue: [ ^false ] ].
	^true
]

{ #category : #testing }
Integer >> isProbablyPrime [
	"See isProbablyPrimeWithK:andQ:randomIndex: for the algoritm description."
	
	| k q randomNumbers |
	self <= 1 ifTrue: [ ^false ].
	self even ifTrue: [ ^self = 2 ].
	"Factor self into (2 raisedTo: k) * q + 1, where q odd"
	q := self bitShift: -1.
	k := q lowBit.
	q := q bitShift: 1 - k.
	"Repeat the probabilistic until false (the probability of false negative is null) or until probability is very low."
	
	"Array of pregenerated 25 random numbers. "
	randomNumbers := #(0.816980664067427 0.9940209812456839 0.5106317962103671
		0.18859890764048273 0.7818407135931034 0.39687335928756434
		0.25054954609393587 0.9862212007801148 0.41972151138806785
		0.25944189925652084 0.44000080434605515 0.09351864414919105
		0.7678522154539136 0.292185133924794 0.7555458740124227
		0.4595045267881381 0.8925817282370206 0.621106479606175 
		0.9366027409846908 0.48226772969694237 0.47373301651036975
		0.030808489784043512 0.7982878004192784 0.8230616468112272
		0.19709795629470514).
	
	randomNumbers do: [:rnd | (self isProbablyPrimeWithK: k andQ: q randomIndex: rnd ) ifFalse: [ ^false ] ].
	"The probability of false positive after 25 iterations is less than (1/4 raisedTo: 25) < 1.0e-15"
	^true
]

{ #category : #testing }
Integer >> isProbablyPrimeWithK: k andQ: q randomIndex: aRandomFloat [
	"Algorithm P, probabilistic primality test, from
	Knuth, Donald E. 'The Art of Computer Programming', Vol 2,
	Third Edition, section 4.5.4, page 395, P1-P5 refer to Knuth description..
	Note that this is a Miller Rabin test which may answer false positives (known as pseudoprimes) for at most 1/4 of the possible bases x."

	| x j y minusOne |
	"P1"
	x := ((self - 2) * aRandomFloat "a pregerated random number") asInteger + 1.	
	"P2"
	j := 0.
	y := x raisedTo: q modulo: self.
	minusOne := self - 1.
	
	["P3"
	y = 1 ifTrue: [^j = 0].
	y = minusOne ifTrue: [^true].
	"P4"
	(j := j + 1) < k]
		whileTrue:
			[y := y squared \\ self].
	"P5"
	^false
]

{ #category : #'system primitives' }
Integer >> lastDigit [
	"Answer the last digit of the integer base 256.  LargePositiveInteger uses bytes of base two number, and each is a 'digit'."

	^self digitAt: self digitLength
]

{ #category : #'mathematical functions' }
Integer >> lcm: n [ 
	"Answer the least common multiple of the receiver and n."

	^self // (self gcd: n) * n
]

{ #category : #'bit manipulation' }
Integer >> lowBit [
	"Answer the index of the low order bit of this number."
	| index |
	self = 0 ifTrue: [ ^ 0 ].
	index := 1.
	[ (self digitAt: index) = 0 ]
		whileTrue:
			[ index := index + 1 ].
	^ (self digitAt: index) lowBit + (8 * (index - 1))
]

{ #category : #private }
Integer >> montgomeryDigitBase [
	"Answer the base used by Montgomery algorithm."
	^1 << self montgomeryDigitLength
]

{ #category : #private }
Integer >> montgomeryDigitLength [
	"Answer the number of bits composing a digit in Montgomery algorithm.
	Primitive use either 8 or 32 bits digits"
	<primitive: 'primMontgomeryDigitLength' module:'LargeIntegers'>
	^8 "Legacy plugin which did not have this primitive did use 8 bits digits"
]

{ #category : #private }
Integer >> montgomeryDigitMax [
	"Answer the maximum value of a digit used in Montgomery algorithm."
	
	^1 << self montgomeryDigitLength - 1
]

{ #category : #private }
Integer >> montgomeryNumberOfDigits [
	"Answer the number of montgomery digits required to represent the receiver."
	^self digitLength * 8 + (self montgomeryDigitLength - 1) // self montgomeryDigitLength
]

{ #category : #private }
Integer >> montgomeryRaisedTo: n times: y modulo: m mInvModB: mInv [
	"Private - do a Montgomery exponentiation of self modulo m.
	The operation is equivalent to (self/y raisedTo: n)*y \\ m,
	with y is (b raisedTo: m montgomeryNumberOfDigits),
	with (m bitAnd: b-1) * mInv \\ b = (b-1)
	with b = self montgomeryDigitBase (either 1<<8 or 1<<32)"
	
	| pow j k w index oddPowersOfSelf square |
	
	"Precompute powers of self for odd bit patterns xxxx1 up to length w + 1.
	The width w is chosen with respect to the total bit length of n,
	such that each bit pattern will on average be encoutered P times in the whole bit sequence of n.
	This costs (2 raisedTo: w) multiplications, but more will be saved later (see below)."
	k := n highBit.
	w := (k highBit - 1 >> 1 min: 16) max: 1.
	oddPowersOfSelf := Array new: 1 << w.
	oddPowersOfSelf at: 1 put: (pow := self).
	square := self montgomeryTimes: self modulo: m mInvModB: mInv.
	2 to: oddPowersOfSelf size do: [:i | pow := oddPowersOfSelf at: i put: (pow montgomeryTimes: square modulo: m mInvModB: mInv)].
	
	"Now exponentiate by searching precomputed bit patterns with a sliding window"
	pow := y.
	[k > 0]
		whileTrue:
			[pow := pow montgomeryTimes: pow modulo: m mInvModB: mInv.
			"Skip bits set to zero (the sliding window)"
			(n bitAt: k) = 0
				ifFalse:
					["Find longest odd bit pattern up to window length (w + 1)"
					j := k - w max: 1.
					[j < k and: [(n bitAt: j) = 0]] whileTrue: [j := j + 1].
					"We found a bit pattern of length k-j+1;
					perform the square powers for each bit
					(same cost as bitwise algorithm);
					compute the index of this bit pattern in the precomputed powers."
					index := 0.
					[k > j] whileTrue:
						[pow := pow montgomeryTimes: pow modulo: m mInvModB: mInv.
						index := index << 1 + (n bitAt: k).
						k := k - 1].
					"Perform a single multiplication for the whole bit pattern.
					This saves up to (k-j) multiplications versus a naive algorithm operating bit by bit"
					pow := pow montgomeryTimes: (oddPowersOfSelf at: index + 1) modulo: m mInvModB: mInv].
			k := k - 1].
	^pow
]

{ #category : #private }
Integer >> montgomeryTimes: a modulo: m mInvModB: mInv [
	"Answer the result of a Montgomery multiplication
	self * a * (b raisedTo: m montgomeryNumberOfDigits) inv \\ m
	NOTE: it is assumed that:
	self montgomeryNumberOfDigits <= m montgomeryNumberOfDigits
	a montgomeryNumberOfDigits <= m montgomeryNumberOfDigits
	mInv * m \\ b = (-1 \\ b) = (b-1) (this implies m odd)
	where b = self montgomeryDigitBase
	
	Answer nil in case of absent plugin or other failure."
	
	<primitive: 'primMontgomeryTimesModulo' module:'LargeIntegers'>
	^nil
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

{ #category : #'mathematical functions' }
Integer >> nthRoot: aPositiveInteger [
	"Answer the nth root of the receiver.
	Answer an Integer if root is exactly this Integer, else answer the Float nearest the exact root."

	| guess p |

	guess := self nthRootRounded: aPositiveInteger.
	(guess raisedTo: aPositiveInteger) = self
		ifTrue: [ ^ guess ].

	p := Float precision - guess highBitOfMagnitude.
	p < 0 ifTrue: [ ^ guess asFloat ].

	guess := self << (p * aPositiveInteger) nthRootRounded: aPositiveInteger.
	^(guess / (1 << p)) asFloat
]

{ #category : #'mathematical functions' }
Integer >> nthRootRounded: aPositiveInteger [
	"Answer the integer nearest the nth root of the receiver."
	| guess |
	self = 0 ifTrue: [^0].
	self negative
		ifTrue:
			[aPositiveInteger even ifTrue: [ ArithmeticError signal: 'Negative numbers don''t have even roots.' ].
			^(self negated nthRootRounded: aPositiveInteger) negated].
	guess := self nthRootTruncated: aPositiveInteger.
	^self * 2 > ((guess + 1 raisedTo: aPositiveInteger) + (guess raisedTo: aPositiveInteger))
		ifTrue: [guess + 1]
		ifFalse: [guess]
]

{ #category : #'mathematical functions' }
Integer >> nthRootTruncated: aPositiveInteger [
	"Answer the integer part of the nth root of the receiver."
	| guess guessToTheNthMinusOne delta |
	self = 0 ifTrue: [^0].
	self negative
		ifTrue:
			[aPositiveInteger even ifTrue: [ ArithmeticError signal: 'Negative numbers don''t have even roots.' ].
			^(self negated nthRootTruncated: aPositiveInteger) negated].
	guess := 1 bitShift: self highBitOfMagnitude + aPositiveInteger - 1 // aPositiveInteger.
	[
		guessToTheNthMinusOne := guess raisedTo: aPositiveInteger - 1.
		delta := (guess * guessToTheNthMinusOne - self) // (guessToTheNthMinusOne * aPositiveInteger).
		delta = 0 ] whileFalse:
			[ guess := guess - delta ].
	( (guess := guess - 1) raisedTo: aPositiveInteger) > self  ifTrue:
			[ guess := guess - 1 ].
	^guess
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

{ #category : #accessing }
Integer >> primeFactors [
	"Return all primeFactors of myself"
	"( 106260 primeFactors fold: [ :a :b | a * b ]) = 106260"
	
	^ Array streamContents: [ :s | self primeFactorsOn: s ]
]

{ #category : #accessing }
Integer >> primeFactorsOn: aStream [
	"Recursively calculate the primefactors of myself and put the factors into the given stream"
	self = 1 
		ifTrue: [ ^ self ].
		
	self even ifTrue: [
		aStream nextPut: 2.
		^ (self / 2) primeFactorsOn: aStream ].
	
	3 to: self sqrtFloor by: 2 do: [ :each |
		self \\ each = 0 
			ifTrue: [ 
				aStream nextPut: each.
				^ (self / each) primeFactorsOn: aStream ]].
	
	aStream nextPut: self.
]

{ #category : #private }
Integer >> print: positiveNumberString on: aStream prefix: prefix length: minimum padded: zeroFlag [
	| padLength |
	padLength := minimum - positiveNumberString size - prefix size.
	padLength > 0
		ifTrue: [zeroFlag
				ifTrue: [aStream nextPutAll: prefix; nextPutAll: (String new: padLength withAll: $0)]
				ifFalse: [aStream nextPutAll: (String new: padLength withAll: Character space); nextPutAll: prefix]]
		ifFalse: [aStream nextPutAll: prefix].
	aStream nextPutAll: positiveNumberString
	
]

{ #category : #printing }
Integer >> printOn: aStream [
	^self printOn: aStream base: 10
]

{ #category : #printing-numerative }
Integer >> printOn: aStream base: base length: minimum padded: zeroFlag [
	| prefix |
	
	prefix := self negative ifTrue: ['-'] ifFalse: [ '' ].
	
	self print: (self abs printStringBase: base) on: aStream prefix: prefix length: minimum padded: zeroFlag

]

{ #category : #printing-numerative }
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
	
	self sign = -1 
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

{ #category : #printing-numerative }
Integer >> printStringBase: base length: minimum padded: zeroFlag [
	^String streamContents: [:s| self printOn: s base: base length: minimum padded: zeroFlag]
]

{ #category : #printing-numerative }
Integer >> printStringHex [
	"returns the hex digit part of the integer when printed in hexadecimal format.
	30 printStringHex 
		'1E'
	30 hex
		 '16r1E' "
		
	^self printStringBase: 16
]

{ #category : #printing-numerative }
Integer >> printStringLength: minimal [
	^self printStringLength: minimal padded: false

]

{ #category : #printing-numerative }
Integer >> printStringLength: minimal padded: zeroFlag [
	^self printStringBase: 10 length: minimal padded: zeroFlag
]

{ #category : #printing-numerative }
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

{ #category : #printing-numerative }
Integer >> printStringRoman [
	| stream integer |
	stream := String new writeStream.
	integer := self negative ifTrue: [stream nextPut: $-. self negated] ifFalse: [self].
	integer // 1000 timesRepeat: [stream nextPut: $M].
	integer
		romanDigits: 'MDC' for: 100 on: stream;
		romanDigits: 'CLX' for: 10 on: stream;
		romanDigits: 'XVI' for: 1 on: stream.
	^stream contents
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

{ #category : #streaming }
Integer >> putOn: aStream [
	aStream isBinary
		ifTrue: [ self asByteArray do: [ :each | aStream nextPut: each ] ]
		ifFalse: [ self asString putOn: aStream ]
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

{ #category : #printing-numerative }
Integer >> radix: base [ 
	^ self printStringBase: base
]

{ #category : #'mathematical functions' }
Integer >> raisedTo: n modulo: m [
	"Answer the modular exponential.
	Note: this implementation is optimized for case of large integers raised to large powers."
	| a s mInv |
	n = 0 ifTrue: [^1].
	(self >= m or: [self < 0]) ifTrue: [^self \\ m raisedTo: n modulo: m].
	n < 0 ifTrue: [^(self reciprocalModulo: m) raisedTo: n negated modulo: m].
	(n < 4096 or: [m even])
		ifTrue:
			["Overhead of Montgomery method might cost more than naive divisions, use naive"
			^self slidingLeftRightRaisedTo: n modulo: m].
	
	mInv := self montgomeryDigitBase - ((m bitAnd: self montgomeryDigitMax) reciprocalModulo: self montgomeryDigitBase).
 
	"Initialize the result to R=self montgomeryDigitModulo raisedTo: m montgomeryNumberOfDigits"
	a := (1 bitShift: m montgomeryNumberOfDigits * m montgomeryDigitLength) \\ m.
	
	"Montgomerize self (multiply by R)"
	(s := self montgomeryTimes: (a*a \\ m) modulo: m mInvModB: mInv)
		ifNil:
			["No Montgomery primitive available ? fallback to naive divisions"
			^self slidingLeftRightRaisedTo: n modulo: m].

	"Exponentiate self*R"
	a := s montgomeryRaisedTo: n times: a modulo: m mInvModB: mInv.

	"Demontgomerize the result (divide by R)"
	^a montgomeryTimes: 1 modulo: m mInvModB: mInv
]

{ #category : #'mathematical functions' }
Integer >> raisedToInteger: exp modulo: m [
	(exp = 0) ifTrue: [^ 1].
	exp even
		ifTrue: [^ (self raisedToInteger: (exp // 2) modulo: m) squared \\ m]
		ifFalse: [^ (self * (self raisedToInteger: (exp - 1) modulo: m)) \\ m].
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
		self digitAt: i put: (replacement digitAt: j).
		j := j+1]
]

{ #category : #private }
Integer >> romanDigits: digits for: base on: aStream [
	| n |
	n := self \\ (base * 10) // base.
	n = 9 ifTrue: [^ aStream nextPut: digits last; nextPut: digits first].
	n = 4 ifTrue: [^ aStream nextPut: digits last; nextPut: digits second].
	n > 4 ifTrue: [aStream nextPut: digits second].
	n \\ 5 timesRepeat: [aStream nextPut: digits last]
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

{ #category : #private }
Integer >> slidingLeftRightRaisedTo: n modulo: m [
	"Private - compute (self raisedTo: n) \\ m,
	Note: this method has to be fast because it is generally used with large integers in cryptography.
	It thus operate on exponent bits from left to right by packets with a sliding window rather than bit by bit (see below)."
	
	| pow j k w index oddPowersOfSelf square |
	
	"Precompute powers of self for odd bit patterns xxxx1 up to length w + 1.
	The width w is chosen with respect to the total bit length of n,
	such that each bit pattern will on average be encoutered P times in the whole bit sequence of n.
	This costs (2 raisedTo: w) multiplications, but more will be saved later (see below)."
	k := n highBit.
	w := (k highBit - 1 >> 1 min: 16) max: 1.
	oddPowersOfSelf := Array new: 1 << w.
	oddPowersOfSelf at: 1 put: (pow := self).
	square := self * self \\ m.
	2 to: oddPowersOfSelf size do: [:i | pow := oddPowersOfSelf at: i put: pow * square \\ m].
	
	"Now exponentiate by searching precomputed bit patterns with a sliding window"
	pow := 1.
	[k > 0]
		whileTrue:
			[pow := pow * pow \\ m.
			"Skip bits set to zero (the sliding window)"
			(n bitAt: k) = 0
				ifFalse:
					["Find longest odd bit pattern up to window length (w + 1)"
					j := k - w max: 1.
					[j < k and: [(n bitAt: j) = 0]] whileTrue: [j := j + 1].
					"We found an odd bit pattern of length k-j+1;
					perform the square powers for each bit
					(same cost as bitwise algorithm);
					compute the index of this bit pattern in the precomputed powers."
					index := 0.
					[k > j] whileTrue:
						[pow := pow * pow \\ m.
						index := index << 1 + (n bitAt: k).
						k := k - 1].
					"Perform a single multiplication for the whole bit pattern.
					This saves up to (k-j) multiplications versus a naive algorithm operating bit by bit"
					pow := pow * (oddPowersOfSelf at: index + 1) \\ m].
			k := k - 1].
	^pow
]

{ #category : #'mathematical functions' }
Integer >> sqrt [
	"Answer the square root of the receiver."

	| selfAsFloat floatResult guess |
	selfAsFloat := self asFloat.
	floatResult := selfAsFloat sqrt.

	floatResult isInfinite ifFalse: [
		guess := floatResult truncated.

		"If got an exact answer, answer it. Otherwise answer float approximate answer."
		guess squared = self
			ifTrue: [ ^ guess ]].

	"In this case, maybe it failed because we are such a big integer that the Float method becomes
	inexact, even if we are a whole square number. So, try the slower but more general method"
	selfAsFloat >= Float maxExactInteger asFloat squared
		ifTrue: [
			guess := self sqrtFloor.
			guess squared = self ifTrue: [
				^guess ].
			
			"Nothing else can be done. No exact answer means answer must be a Float.
			Answer the best we have which is the rounded sqrt."
			guess := (self * 4) sqrtFloor.
			^(guess // 2 + (guess \\ 2)) asFloat].

	"We need an approximate result"
	^floatResult
]

{ #category : #testing }
Integer >> sqrtFloor [
	"Return the integer part of the square root of self"

	| guess guessSquared delta |
	guess := 1 bitShift: self highBit + 1 // 2.
	[
		guessSquared := guess * guess.
		delta := guessSquared - self // (guess bitShift: 1).
		delta = 0 ] whileFalse: [
			guess := guess - delta ].
	guessSquared = self ifFalse: [ guess := guess - 1 ].
	^guess
]

{ #category : #printing-numerative }
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

{ #category : #printing-numerative }
Integer >> storeOn: aStream base: base length: minimum padded: zeroFlag [
	| prefix |
	prefix := self negative ifTrue: ['-'] ifFalse: [String new].
	base = 10 ifFalse: [prefix := prefix, base printString, 'r'].
	self print: (self abs printStringBase: base) on: aStream prefix: prefix length: minimum padded: zeroFlag

]

{ #category : #printing-numerative }
Integer >> storeStringBase: base length: minimum padded: zeroFlag [
	^String streamContents: [:s| 
		self storeOn: s base: base length: minimum padded: zeroFlag ]
]

{ #category : #printing-numerative }
Integer >> storeStringHex [
	^self storeStringBase: 16
]

{ #category : #'mathematical functions' }
Integer >> take: kk [
	"Return the number of combinations of (self) elements taken kk at a time.  
	For 6 take 3, this is 6*5*4 / (1*2*3).  Zero outside of Pascal's triangle.  Use a trick to go faster."
	" 6 take: 3  "

	| num denom |
	kk < 0 ifTrue: [^ 0].
	kk > self ifTrue: [^ 0].
	num := 1.
	self to: (kk max: self-kk) + 1 by: -1 do: [:factor | num := num * factor].
	denom := 1.
	1 to: (kk min: self-kk) do: [:factor | denom := denom * factor].
	^ num // denom
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

{ #category : #benchmarks }
Integer >> tinyBenchmarks [
	"Report the results of running the two tiny Squeak benchmarks.
	ar 9/10/1999: Adjusted to run at least 1 sec to get more stable results"
	"0 tinyBenchmarks"
	"On a 292 MHz G3 Mac: 22727272 bytecodes/sec; 984169 sends/sec"
	"On a 400 MHz PII/Win98:  18028169 bytecodes/sec; 1081272 sends/sec"
	| t1 t2 r n1 n2 |
	n1 := 1.
	[t1 := Time millisecondsToRun: [n1 benchmark].
	t1 < 1000] whileTrue:[n1 := n1 * 2]. "Note: #benchmark's runtime is about O(n)"

	n2 := 28.
	[t2 := Time millisecondsToRun: [r := n2 benchFib].
	t2 < 1000] whileTrue:[n2 := n2 + 1]. "Note: #benchFib's runtime is about O(n^2)."

	^ ((n1 * 500000 * 1000) // t1) printString, ' bytecodes/sec; ',
	  ((r * 1000) // t2) printString, ' sends/sec'
]

{ #category : #'truncation and round off' }
Integer >> truncated [ 
	"Refer to the comment in Number|truncated."
]
