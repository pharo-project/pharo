"
Fraction provides methods for dealing with fractions like 1/3 as fractions (not as 0.33333...).  All public arithmetic operations answer reduced fractions (see examples).

instance variables: 'numerator denominator '

Examples: (note the parentheses required to get the right answers in Smalltalk and Pharo):

(2/3) + (2/3)
(2/3) + (1/2)		 ""answers shows the reduced fraction"" 
(2/3) raisedToInteger: 5		 ""fractions also can have exponents""

"
Class {
	#name : #Fraction,
	#superclass : #Number,
	#instVars : [
		'numerator',
		'denominator'
	],
	#category : #'Kernel-Numbers'
}

{ #category : #'instance creation' }
Fraction class >> numerator: numInteger denominator: denInteger [ 
	"Answer an instance of me (numInteger/denInteger).
	NOTE: This primitive initialization method will not reduce improper fractions,
	so normal usage should be coded as, eg,
		(Fraction numerator: a denominator: b) reduced
	or, more simply, as
		a / b."

	^self new setNumerator: numInteger denominator: denInteger
]

{ #category : #arithmetic }
Fraction >> * aNumber [ 
	"Answer the result of multiplying the receiver by aNumber."
	| d1 d2 |
	aNumber isFraction ifTrue: 
		[d1 := numerator gcd: aNumber denominator.
		d2 := denominator gcd: aNumber numerator.
		(d2 = denominator and: [d1 = aNumber denominator])
			ifTrue: [^ numerator // d1 * (aNumber numerator // d2)].
		^ Fraction numerator: numerator // d1 * (aNumber numerator // d2)
				denominator: denominator // d2 * (aNumber denominator // d1)].
	^ aNumber adaptToFraction: self andSend: #*
]

{ #category : #arithmetic }
Fraction >> + aNumber [ 
	"Answer the sum of the receiver and aNumber."
	| n d d1 d2 |
	aNumber isInteger ifTrue:
		[^Fraction numerator: numerator + (denominator * aNumber) denominator: denominator].
	aNumber isFraction ifTrue: 
		[d := denominator gcd: aNumber denominator.
		n := numerator * (d1 := aNumber denominator // d) + (aNumber numerator * (d2 := denominator // d)).
		d1 := d1 * d2.
		n := n // (d2 := n gcd: d).
		(d := d1 * (d // d2)) = 1 ifTrue: [^ n].
		^ Fraction numerator: n denominator: d].
	^ aNumber adaptToFraction: self andSend: #+
]

{ #category : #arithmetic }
Fraction >> - aNumber [
	"Answer the difference between the receiver and aNumber."
	aNumber isInteger ifTrue:
		[^Fraction numerator: numerator - (denominator * aNumber) denominator: denominator].
	aNumber isFraction ifTrue:
		[^ self + aNumber negated].
	^ aNumber adaptToFraction: self andSend: #-
]

{ #category : #arithmetic }
Fraction >> / aNumber [
	"Answer the result of dividing the receiver by aNumber."
	aNumber isFraction
		ifTrue: [^self * aNumber reciprocal].
	^ aNumber adaptToFraction: self andSend: #/
]

{ #category : #comparing }
Fraction >> < aNumber [
	aNumber isFraction ifTrue:
		[^ numerator * aNumber denominator < (aNumber numerator * denominator)].
	^ aNumber adaptToFraction: self andCompare: #<
]

{ #category : #comparing }
Fraction >> <= aNumber [
	aNumber isFraction ifTrue:
		[^ numerator * aNumber denominator <= (aNumber numerator * denominator)].
	^ aNumber adaptToFraction: self andCompare: #<=
]

{ #category : #comparing }
Fraction >> = aNumber [
	aNumber isNumber ifFalse: [^ false].
	aNumber isFraction
		ifTrue: [numerator = 0 ifTrue: [^ aNumber numerator = 0].
				^ (numerator * aNumber denominator) =
					(aNumber numerator * denominator)
				"Note: used to just compare num and denom,
					but this fails for improper fractions"].
	^ aNumber adaptToFraction: self andCompare: #=
]

{ #category : #comparing }
Fraction >> > aNumber [
	aNumber isFraction ifTrue:
		[^ numerator * aNumber denominator > (aNumber numerator * denominator)].
	^ aNumber adaptToFraction: self andCompare: #>
]

{ #category : #comparing }
Fraction >> >= aNumber [
	aNumber isFraction ifTrue:
		[^ numerator * aNumber denominator >= (aNumber numerator * denominator)].
	^ aNumber adaptToFraction: self andCompare: #>=
]

{ #category : #converting }
Fraction >> adaptToInteger: rcvr andSend: selector [
	"If I am involved in arithmetic with an Integer, convert it to a Fraction."
	^ (Fraction numerator: rcvr denominator: 1) perform: selector with: self
]

{ #category : #converting }
Fraction >> asFloat [
	"Answer a Float that closely approximates the value of the receiver.
	This implementation will answer the closest floating point number to the receiver.
	In case of a tie, it will use the IEEE 754 round to nearest even mode.
	In case of overflow, it will answer +/- Float infinity."

	| a b mantissa exponent hasTruncatedBits lostBit n ha hb hm |
	a := numerator abs.
	b := denominator.	"denominator is always positive"
	ha := a highBitOfMagnitude.
	hb := b highBitOfMagnitude.
	
	"Number of bits to keep in mantissa plus one to handle rounding."
	n := 1 + Float precision.

	"If both numerator and denominator are represented exactly in floating point number,
	then fastest thing to do is to use hardwired float division."
	(ha < n and: [hb < n]) ifTrue: [^numerator asFloat / denominator asFloat].

	"Shift the fraction by a power of two exponent so as to obtain a mantissa with n bits.
	First guess is rough, the mantissa might have n+1 bits."
	exponent := ha - hb - n.
	exponent >= 0
		ifTrue: [b := b bitShift: exponent]
		ifFalse: [a := a bitShift: exponent negated].
	mantissa := a quo: b.
	hasTruncatedBits := a > (mantissa * b).
	hm := mantissa highBit.
	
	"Check for gradual underflow, in which case the mantissa will loose bits.
	Keep at least one bit to let underflow preserve the sign of zero."
	lostBit := Float emin - (exponent + hm - 1).
	lostBit > 0 ifTrue: [n := n - lostBit max: 1].

	"Remove excess bits in the mantissa."
	hm > n
		ifTrue:
			[exponent := exponent + hm - n.
			hasTruncatedBits := hasTruncatedBits or: [mantissa anyBitOfMagnitudeFrom: 1 to: hm - n].
			mantissa := mantissa bitShift: n - hm].

	"Check if mantissa must be rounded upward.
	The case of tie (mantissa odd & hasTruncatedBits not)
	will be handled by Integer>>asFloat."
	(hasTruncatedBits and: [mantissa odd])
		ifTrue: [mantissa := mantissa + 1].

	^ (self positive
			ifTrue: [mantissa asFloat]
			ifFalse: [mantissa asFloat negated])
		timesTwoPower: exponent
]

{ #category : #converting }
Fraction >> asFraction [	
	"Answer the receiver itself."

	^self
]

{ #category : #'truncation and round off' }
Fraction >> asLargerPowerOfTwo [
	"Convert the receiver into a power of two which is not less than the receiver"
	| quotient |
	(numerator = 0 or: [numerator sign ~= denominator sign]) ifTrue: [^DomainError signal: 'Value outside (0 , infinity)' from: 0].
	^(quotient := denominator // numerator) > 0 
		ifTrue: [Fraction numerator: 1 denominator:  (1 bitShift: (quotient highBit -1))]
		ifFalse: [quotient := numerator // denominator.
				"If my quotient is a power of two, we, we need to check remainder, to see if we should shift by highbit or not.
				 (This is equivalent to Integer asLargerPowerOfTwo returning self when receiver is power of two) "
				(quotient isPowerOfTwo and: [numerator \\ denominator = 0])
					ifTrue: [quotient]
					ifFalse: [1 bitShift: (quotient highBit )]]
]

{ #category : #converting }
Fraction >> asScaledDecimal [
	"Convert the receiver to a ScaledDecimal.
	If there is a finite decimal representation of the receiver, then use the exact number of decimal places required.
	Else, use a default number of decimals."
	
	| pow2 pow5 q q5 |
	pow2 := denominator lowBit - 1.
	q := denominator bitShift: pow2 negated.
	pow5 := 0.
	[q = 1]
		whileFalse: [
			q5 := q // 5.
			(q - (5 * q5)) = 0 ifFalse: [^super asScaledDecimal].
			q := q5.
			pow5 := pow5 + 1].
	^self asScaledDecimal: (pow2 max: pow5)
]

{ #category : #'truncation and round off' }
Fraction >> asSmallerPowerOfTwo [
	"Convert the receiver into a power of two which is not larger than the receiver"
	| quotient |
	(numerator = 0 or: [numerator sign ~= denominator sign]) ifTrue: [^DomainError signal: 'Value outside (0 , infinity)' from: 0].
	^(quotient := denominator // numerator) > 0 
		ifTrue: [	"If my quotient is a power of two, we, we need to check remainder, to see if we should shift by highbit or not.
				 (This is equivalent to Integer asSmallerPowerOfTwo returning self when receiver is power of two) "
				 (quotient isPowerOfTwo and: [denominator \\ numerator = 0]) 
					ifTrue: [Fraction numerator: 1 denominator: quotient]
					ifFalse:[Fraction numerator: 1 denominator:  (1 bitShift: quotient highBit)]]
					
		ifFalse: [1 bitShift: ((numerator // denominator) highBit -1)]
]

{ #category : #private }
Fraction >> denominator [

	^denominator
]

{ #category : #comparing }
Fraction >> hash [
	"Hash is reimplemented because = is implemented.
	Care is taken that a Fraction equal to a Float also have an equal hash"

	| tmp |
	denominator isPowerOfTwo ifTrue: [
		"If denominator is a power of two, I can be exactly equal to a Float"
		tmp := self asFloat.
		tmp isFinite ifTrue: [^tmp hash]].
	
	"Else, I cannot be exactly equal to a Float, use own hash algorithm.
	(Assume the fraction is already reduced)"
	^numerator hash bitXor: denominator hash
]

{ #category : #converting }
Fraction >> isFraction [
	^ true
]

{ #category : #testing }
Fraction >> isPowerOfTwo [
	^ numerator = 1 and: [ denominator isPowerOfTwo ]
]

{ #category : #'self evaluating' }
Fraction >> isSelfEvaluating [ 
	^ true
]

{ #category : #'mathematical functions' }
Fraction >> ln [
	"This function is defined because super ln might overflow."
	| res |
	self <= 0 ifTrue: [^DomainError signal: 'ln is only defined for x > 0' from: 0].
	"Test self < 1 before converting to float in order to avoid precision loss due to gradual underflow."
	numerator < denominator ifTrue: [^self reciprocal ln negated].
	res := super ln.
	res isFinite ifTrue: [^res].
	^numerator ln - denominator ln
]

{ #category : #'mathematical functions' }
Fraction >> log [
	"This function is defined because super log might overflow."
	| res |
	self <= 0 ifTrue: [^DomainError signal: 'log is only defined for x > 0' from: 0].
	"Test self < 1 before converting to float in order to avoid precision loss due to gradual underflow."
	numerator < denominator ifTrue: [^self reciprocal log negated].
	res := super log.
	res isFinite ifTrue: [^res].
	^numerator log - denominator log
]

{ #category : #arithmetic }
Fraction >> negated [ 
	"Refer to the comment in Number|negated."

	^ Fraction
		numerator: numerator negated
		denominator: denominator
]

{ #category : #testing }
Fraction >> negative [

	^numerator negative
]

{ #category : #'mathematical functions' }
Fraction >> nthRoot: aPositiveInteger [
	"Answer the nth root of the receiver."

	| guess |
	guess := (numerator nthRootTruncated: aPositiveInteger) / (denominator nthRootTruncated: aPositiveInteger).
	(guess raisedTo: aPositiveInteger) = self ifTrue: [^guess].
	"There is no exact nth root, so answer a Float approximation"
	^(self abs ln / aPositiveInteger) exp * self sign
]

{ #category : #private }
Fraction >> numerator [

	^numerator
]

{ #category : #printing }
Fraction >> printOn: aStream [

	aStream nextPut: $(.
	numerator printOn: aStream.
	aStream nextPut: $/.
	denominator printOn: aStream.
	aStream nextPut: $).

]

{ #category : #printing }
Fraction >> printOn: aStream base: base [

	aStream nextPut: $(.
	numerator printOn: aStream base: base.
	aStream nextPut: $/.
	denominator printOn: aStream base: base.
	aStream nextPut: $).

]

{ #category : #printing }
Fraction >> printOn: aStream showingDecimalPlaces: placesDesired [
	"Same as super, but provides a faster implementation by inlining some Fraction protocol thus avoiding intermediate Fraction creation."
	
	| roundedFractionPart integerPart scaling |
	placesDesired <= 0
		ifTrue: [self rounded printOn: aStream]
		ifFalse:
			[scaling := 10 raisedToInteger: placesDesired.
			integerPart := numerator abs quo: denominator.
			roundedFractionPart := (numerator abs - (integerPart * denominator)) * scaling * 2 + denominator quo: denominator * 2.
			roundedFractionPart = scaling
				ifTrue:
					[integerPart := integerPart + 1.
					roundedFractionPart := 0].
			"Don't print minus sign if result is rouded to zero"
			(numerator negative and: [integerPart > 0 or: [roundedFractionPart > 0]]) ifTrue: [aStream nextPut: $-].
			integerPart printOn: aStream.
			aStream nextPut: $..
			roundedFractionPart printOn: aStream base: 10 length: placesDesired padded: true].
]

{ #category : #'mathematical functions' }
Fraction >> raisedToFraction: aFraction [
	| root |
	root := (self numerator nthRootTruncated: aFraction denominator) / (self denominator nthRootTruncated: aFraction denominator).
	(root raisedToInteger: aFraction denominator) = self ifTrue: [^root raisedToInteger: aFraction numerator].
	^super raisedToFraction: aFraction
]

{ #category : #'mathematical functions' }
Fraction >> raisedToInteger: anInteger [ 
	"See Number | raisedToInteger:"
	anInteger = 0 ifTrue: [^ 1].
	anInteger < 0 ifTrue: [^ self reciprocal raisedToInteger: anInteger negated].
	^ Fraction numerator: (numerator raisedToInteger: anInteger)
		denominator: (denominator raisedToInteger: anInteger)
]

{ #category : #private }
Fraction >> reciprocal [
	
	numerator abs = 1 ifTrue: [^denominator * numerator].
	^self class numerator: denominator denominator: numerator
]

{ #category : #private }
Fraction >> reduced [

	| gcd numer denom |
	numerator = 0 ifTrue: [^0].
	gcd := numerator gcd: denominator.
	numer := numerator // gcd.
	denom := denominator // gcd.
	denom = 1 ifTrue: [^numer].
	^Fraction numerator: numer denominator: denom
]

{ #category : #'truncation and round off' }
Fraction >> round: numberOfWishedDecimal [
	"Round the decimal part of the receiver to be limited to the number of wished decimal. Only leave a fixed amount of decimal"
   "(1/3 round: 2) >>> (33/100) "   
	"(111/100 round: 2) >>> (111/100) "
	
	^self roundTo: (10 raisedTo: numberOfWishedDecimal negated)
]

{ #category : #private }
Fraction >> setNumerator: n denominator: d [

	d = 0
		ifTrue: [^(ZeroDivide dividend: n) signal]
		ifFalse: 
			[numerator := n asInteger.
			denominator := d asInteger abs. "keep sign in numerator"
			d < 0 ifTrue: [numerator := numerator negated]]
]

{ #category : #'mathematical functions' }
Fraction >> sqrt [
	| d n |
	n := numerator sqrt.
	d := denominator sqrt.
	"The #sqrt method in integer will only answer a Float if there's no exact square root.
	So, we need a float anyway."
	(n isInfinite or: [ d isInfinite ]) ifTrue: [
		^self asFloat sqrt ].
	^n / d
]

{ #category : #'mathematical functions' }
Fraction >> squared [
	"See Fraction (Number) | squared"
	^ Fraction numerator: numerator squared denominator: denominator squared
]

{ #category : #printing }
Fraction >> storeOn: aStream base: base [

	aStream nextPut: $(.
	numerator storeOn: aStream base: base.
	aStream nextPut: $/.
	denominator storeOn: aStream base: base.
	aStream nextPut: $).

]

{ #category : #'truncation and round off' }
Fraction >> truncated [ 
	"Refer to the comment in Number|truncated."

	^numerator quo: denominator
]
