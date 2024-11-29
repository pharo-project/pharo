"
NumberParser implements parsing and building numbers from string/stream.
It offers a framework with utility methods and exception handling.

Instance variables:
sourceStream <Stream> the stream of characters from which the number is read
base <Integer> the radix in which to interpret digits
neg <Boolean> true in case of minus sign
integerPart <Integer> the integer part of the number
fractionPart <Integer> the fraction part of the number if any
exponent <Integer> the exponent used in scientific notation if any
scale <Integer> the scale used in case of ScaledDecimal number if any
nDigits <Integer> number of digits read to form an Integer
lasNonZero <Integer> position of last non zero digit, starting at 1 from left, 0 if all digits are zero
failBlock <BlockClosure> Block to execute whenever an error occurs
"
Class {
	#name : 'NumberParser',
	#superclass : 'Object',
	#instVars : [
		'sourceStream',
		'base',
		'neg',
		'integerPart',
		'fractionPart',
		'exponent',
		'scale',
		'nDigits',
		'lastNonZero',
		'failBlock',
		'maxExponent'
	],
	#classVars : [
		'MaxExponent'
	],
	#category : 'NumberParser-Base',
	#package : 'NumberParser',
	#tag : 'Base'
}

{ #category : 'testing' }
NumberParser class >> isNumber: aStringOrStream [

	| stream |
	stream := aStringOrStream readStream.
	self parse: stream onError: [ ^ false ].
	^ stream atEnd
]

{ #category : 'accessing' }
NumberParser class >> maxExponent [ 

	^ MaxExponent ifNil: [ MaxExponent := 10000 ]
]

{ #category : 'accessing' }
NumberParser class >> maxExponent: aValue [

	MaxExponent := aValue
]

{ #category : 'instance creation' }
NumberParser class >> on: aStringOrStream [
	^self new on: aStringOrStream
]

{ #category : 'instance creation' }
NumberParser class >> parse: aStringOrStream [
	^self new
		on: aStringOrStream;
		nextNumber
]

{ #category : 'instance creation' }
NumberParser class >> parse: aStringOrStream onError: failBlock [
	^(self new)
		on: aStringOrStream;
		failBlock: failBlock;
		nextNumber
]

{ #category : 'accessing' }
NumberParser class >> settingsOn: aBuilder [

	<systemsettings>

	(aBuilder setting: #maxExponent)
		parent: #compiler;
		default: 10000;
		label: 'Max Exponent when parsing numbers';
		description: 'This is the max exponent to parse when parsing numbers in the shape of 1e22. It is the absolute value';
		target: self
]

{ #category : 'instance creation' }
NumberParser class >> squeezeNumberOutOfString: stringOrStream [
	"Try and find a number in this string. First, look if the string
	starts with a number. Then, see if it ends with a number. Then,
	remove a character from the front and see if the remaining
	string makes a number. Repeat the process until no characters
	are left or the number has been found. As soon as a number is
	found, it is returned. Otherwise, the method fails."
	^ self squeezeNumberOutOfString: stringOrStream onError: [self error: 'Reading a number failed']
]

{ #category : 'instance creation' }
NumberParser class >> squeezeNumberOutOfString: stringOrStream onError: errorBlock [
	"Try and find a number in this string. First, look if the string
	starts with a number. Then, see if it ends with a number. Then,
	remove a character from the front and see if the remaining
	string makes a number. Repeat the process until no characters
	are left or the number has been found. As soon as a number is
	found, it is returned. Otherwise, the method fails."

	| string |

	string := stringOrStream.

	stringOrStream size timesRepeat: [
		(self parse: string onError: [ nil ])
			ifNotNil: [ :result| ^ result ].
		string := string allButFirst ].

	^ errorBlock value
]

{ #category : 'accessing' }
NumberParser >> allowPlusSign [
	"Return a boolean indicating if plus sign is allowed or not"

	^false
]

{ #category : 'accessing' }
NumberParser >> allowPlusSignInExponent [
	"Return a boolean indicating if plus sign is allowed or not in exponent"

	^self allowPlusSign
]

{ #category : 'error' }
NumberParser >> expected: aString [

	"This method handle errors on bad syntactically formated numbers.

	Most of the time, the longest valid numeric is parsed and returned.
	Therefore format errors on numbers are rare (arbitrary) occurences (see callers).
	If the first character of a stream is a digit (or a minus followed by a digit), these occurences are even more rare."

	| errorString |

	errorString := aString , ' expected'.
	failBlock ifNotNil: [ ^ failBlock cull: errorString cull: sourceStream position + 1 ].
	self error: 'Reading a number failed: ' , errorString
]

{ #category : 'accessing' }
NumberParser >> exponentLetters [
	"answer the list of possible exponents for Numbers.
	Note: this parser will not honour precision attached to the exponent.
	different exponent do not lead to different precisions.
	only IEEE 754 floating point numbers will be created"

	^'edq'
]

{ #category : 'accessing' }
NumberParser >> failBlock: aBlockOrNil [
	failBlock := aBlockOrNil
]

{ #category : 'private - parsing' }
NumberParser >> makeFloatFromMantissa: m exponent: k base: aRadix [
	"Convert infinite precision arithmetic into Floating point.
	This alogrithm rely on correct IEEE rounding mode
	being implemented in Integer>>asFloat and Fraction>>asFloat"

	^(k positive
		ifTrue: [m * (aRadix raisedToInteger: k)]
		ifFalse: [Fraction numerator: m denominator: (aRadix raisedToInteger: k negated)]) asFloat
]

{ #category : 'private - parsing' }
NumberParser >> makeIntegerOrScaledIntegerOrFloat [
	"At this point, there is no digit, nor fractionPart.
	maybe it can be a scaled decimal with fraction omitted...
	integer with negative exponent can be floats not fractions"

	neg
		ifTrue: [integerPart := integerPart negated].
	self readExponent ifTrue:
		["Check that the result is an integer, otherwise answer a Float.  Fractions are /not/ valid literals."
		 (exponent >= 0 or: [(integerPart * (base raisedToInteger: exponent)) isInteger]) ifFalse:
			[base := base asFloat].
		^integerPart * (base raisedToInteger: exponent)].
	(self readScaleWithDefaultNumberOfDigits: 0)
		ifTrue: [^integerPart asScaledDecimal: scale].
	^ integerPart
]

{ #category : 'private - parsing' }
NumberParser >> makeScaledDecimalWithNumberOfNonZeroFractionDigits: numberOfNonZeroFractionDigits andNumberOfTrailingZeroInFractionPart: numberOfTrailingZeroInFractionPart [
	"At this point integerPart fractionPart and scale have been read out (in inst var).
	Form a ScaledDecimal.
	Care of eliminating trailing zeroes from the fractionPart"

	| decimalMultiplier decimalFraction |
	decimalMultiplier := base raisedToInteger: numberOfNonZeroFractionDigits.
	decimalFraction := integerPart * decimalMultiplier + (fractionPart // (base raisedTo: numberOfTrailingZeroInFractionPart)) / decimalMultiplier.
	neg
		ifTrue: [decimalFraction := decimalFraction negated].
	^decimalFraction asScaledDecimal: scale
]

{ #category : 'accessing' }
NumberParser >> maxExponent [

	^ maxExponent ifNil: [ self class maxExponent ]
]

{ #category : 'accessing' }
NumberParser >> maxExponent: aValue [

	maxExponent  := aValue
]

{ #category : 'private - parsing - large int' }
NumberParser >> nextElementaryLargeIntegerBase: aRadix [
	"Form an unsigned integer with incoming digits from sourceStream.
	Return this integer, or zero if no digits found.
	Stop reading if end of digits or if a LargeInteger is formed.
	Count the number of digits and the position of lastNonZero digit and store them in instVar."

	| value digit char |
	value := 0.
	nDigits := 0.
	lastNonZero := 0.
	[value isLarge or: [(char := sourceStream next) isNil
		or: [digit := char digitValue.
			(0 > digit or: [digit >= aRadix])
				and: [sourceStream skip: -1.
					true]]]]
		whileFalse: [
			nDigits := nDigits + 1.
			0 = digit
				ifFalse: [lastNonZero := nDigits].
			value := value * aRadix + digit].
	^value
]

{ #category : 'parsing' }
NumberParser >> nextFraction [
	| numerator denominator |
	numerator := self nextInteger.
	(sourceStream peekFor: $/) ifFalse: [^numerator].
	denominator := self nextInteger.
	^numerator / denominator
]

{ #category : 'parsing' }
NumberParser >> nextInteger [
	"Main method for reading an Integer.
	This won't try to read a Float nor a ScaledDecimal."

	base := 10.
	neg := self peekSignIsMinus.
	integerPart := self nextUnsignedIntegerOrNilBase: base.
	integerPart ifNil: [ ^self expected: 'Digit'].
	(sourceStream peekFor: $r)
		ifTrue: ["<base>r<integer>"
			(base := integerPart) < 2
				ifTrue: ["A radix currently need to be greater than 1, ungobble the r and return the integer part"
					sourceStream skip: -1.
					^neg
						ifTrue: [base negated]
						ifFalse: [base]].
			self peekSignIsMinus ifTrue: [neg := neg not].
			integerPart := self nextUnsignedIntegerOrNilBase: base.
			integerPart ifNil: [^self expected: 'Digit']].
	neg
		ifTrue: [integerPart := integerPart negated].
	self readExponent
		ifTrue: [^integerPart * (base raisedToInteger: exponent)].
	^ integerPart
]

{ #category : 'parsing' }
NumberParser >> nextIntegerBase: aRadix [
	"Form an integer with following digits.
	Fail if no digit found"

	| isNeg value |
	isNeg := self peekSignIsMinus.
	value := self nextUnsignedIntegerBase: aRadix.
	^isNeg
		ifTrue: [value negated]
		ifFalse: [value]
]

{ #category : 'parsing' }
NumberParser >> nextIntegerBase: aRadix ifFail: aBlock [
	"Form an integer with optional sign and following digits from sourceStream."

	| isNeg value |
	isNeg := self peekSignIsMinus.
	value := self nextUnsignedIntegerOrNilBase: aRadix.
	value ifNil: [^aBlock value].
	^isNeg
		ifTrue: [value negated]
		ifFalse: [value]
]

{ #category : 'private - parsing - large int' }
NumberParser >> nextLargeIntegerBase: aRadix nPackets: nPackets [
	"Form a Large integer with incoming digits from sourceStream.
	Return this integer, or zero if no digits found.
	Stop reading when no more digits or when nPackets elementary LargeInteger have been encountered.
	Count the number of digits and the lastNonZero digit and store them in instVar"

	| high nDigitsHigh lastNonZeroHigh low nDigitsLow halfPackets |
	halfPackets := nPackets bitShift: -1.
	halfPackets = 0 ifTrue: [^self nextElementaryLargeIntegerBase: aRadix].
	high := self nextLargeIntegerBase: aRadix nPackets: halfPackets.
	high isLarge ifFalse: [^high].
	nDigitsHigh := nDigits.
	lastNonZeroHigh := lastNonZero.
	low := self nextLargeIntegerBase: aRadix nPackets: halfPackets.
	nDigitsLow := nDigits.
	nDigits := nDigitsHigh + nDigitsLow.
	lastNonZero := lastNonZero = 0
		ifTrue: [lastNonZeroHigh]
		ifFalse: [lastNonZero + nDigitsHigh].
	^high * (aRadix raisedToInteger: nDigitsLow) + low
]

{ #category : 'parsing' }
NumberParser >> nextNumber [
	"Main method for reading a number.
	 This one can read Float Integer and ScaledDecimal"
	
	| numberOfTrailingZeroInIntegerPart |
	base := 10.
	neg := self peekSignIsMinus.
	integerPart := self nextUnsignedIntegerOrNilBase: base.
	integerPart ifNil: [
		"This is not a regular number beginning with a digit
		It is time to check for exceptional condition NaN and Infinity"
		^self readNamedFloatOrFail].
	numberOfTrailingZeroInIntegerPart := nDigits - lastNonZero.
	(sourceStream peekFor: $r)
		ifTrue: ["<base>r<integer>"
			(base := integerPart) < 2
				ifTrue: [
					sourceStream skip: -1. "Note, the faulty $r is not consumed and is not part or the invalid number."
					^ self expected: 'an integer greater than 1 as valid radix'].
			self peekSignIsMinus
				ifTrue: [neg := neg not].
			integerPart := self nextUnsignedIntegerBase: base.
			numberOfTrailingZeroInIntegerPart := nDigits - lastNonZero].
	^ (sourceStream peekFor: $.)
		ifTrue: [self readNumberWithFractionPartNumberOfTrailingZeroInIntegerPart: numberOfTrailingZeroInIntegerPart]
		ifFalse: [self makeIntegerOrScaledIntegerOrFloat]
]

{ #category : 'parsing' }
NumberParser >> nextNumberBase: b [
	"Method for reading a number without radix prefix.
	This one can read Float Integer and ScaledDecimal"

	| numberOfTrailingZeroInIntegerPart |
	base := b.
	neg := sourceStream peekFor: $-.
	integerPart := self nextUnsignedIntegerOrNilBase: base.
	integerPart ifNil: [
		"This is not a regular number beginning with a digit
		It is time to check for exceptional condition NaN and Infinity"
		^self readNamedFloatOrFail].
	numberOfTrailingZeroInIntegerPart := nDigits - lastNonZero.
	^ (sourceStream peekFor: $.)
		ifTrue: [self readNumberWithFractionPartNumberOfTrailingZeroInIntegerPart: numberOfTrailingZeroInIntegerPart]
		ifFalse: [self makeIntegerOrScaledIntegerOrFloat]
]

{ #category : 'parsing' }
NumberParser >> nextScaledDecimal [
	"Main method for reading a (scaled) decimal number.
	Good Gracious, do not accept a decimal in another base than 10!
	In other words, do not accept radix notation like 2r1.1, even not 10r5.3
	Do not accept exponent notation neither, like 1.0e-3"

	| numberOfNonZeroFractionDigits numberOfTrailingZeroInFractionPart |
	base := 10.
	neg := sourceStream peekFor: $-.
	integerPart := self nextUnsignedIntegerBase: base.
	(sourceStream peekFor: $.)
		ifTrue: [fractionPart := self nextUnsignedIntegerOrNilBase: base.
			fractionPart ifNil: ["Oops, the decimal point seems not part of this number"
							sourceStream skip: -1.
							^ neg
								ifTrue: [integerPart negated asScaledDecimal: 0]
								ifFalse: [integerPart asScaledDecimal: 0]].
			numberOfNonZeroFractionDigits := lastNonZero.
			numberOfTrailingZeroInFractionPart := nDigits - lastNonZero.
			(self readScaleWithDefaultNumberOfDigits: nDigits)
				ifFalse: ["No scale were provided. use number of digits after decimal point as scale"
					scale := nDigits].
			^self makeScaledDecimalWithNumberOfNonZeroFractionDigits: numberOfNonZeroFractionDigits andNumberOfTrailingZeroInFractionPart: numberOfTrailingZeroInFractionPart].
	self readScaleWithDefaultNumberOfDigits: 0.
	neg	ifTrue: [integerPart := integerPart negated].
	^integerPart asScaledDecimal: scale
]

{ #category : 'parsing' }
NumberParser >> nextUnsignedIntegerBase: aRadix [
	"Form an unsigned integer with incoming digits from sourceStream.
	Fail if no digit found.
	Since only characters $A - $Z are supported, limit the character range shown on error.
	Count the number of digits and the lastNonZero digit and store int in instVar "

	| value |
	value := self nextUnsignedIntegerOrNilBase: aRadix.
	value ifNil: [^self expected: ('a digit between 0 and ' copyWith: (Character digitValue: (aRadix min: 36) - 1))].
	^value
]

{ #category : 'parsing' }
NumberParser >> nextUnsignedIntegerBase: aRadix ifFail: errorBlock [
	"Form an unsigned integer with incoming digits from sourceStream.
	Answer this integer, or execute errorBlock if no digit found.
	Count the number of digits and the position of lastNonZero digit and store them in instVar"

	| value |
	value := self nextUnsignedIntegerOrNilBase: aRadix.
	value ifNil: [^errorBlock value].
	^value
]

{ #category : 'parsing' }
NumberParser >> nextUnsignedIntegerOrNilBase: aRadix [
	"Form an unsigned integer with incoming digits from sourceStream.
	Answer this integer, or nil if no digit found.
	Count the number of digits and the position of lastNonZero digit and store them in instVar"

	| nPackets high nDigitsHigh lastNonZeroHigh low |
	"read no more digits than one elementary LargeInteger"
	high :=  self nextElementaryLargeIntegerBase: aRadix.
	nDigits = 0 ifTrue: [^nil].

	"Not enough digits to form a LargeInteger, stop iteration"
	high isLarge ifFalse: [^high].

	"We now have to engage arithmetic with LargeInteger
	Decompose the integer in a high and low packets of growing size:"
	nPackets := 1.
	nDigitsHigh := nDigits.
	lastNonZeroHigh := lastNonZero.
	[
	low := self nextLargeIntegerBase: aRadix nPackets: nPackets .
	high := high * (aRadix raisedToInteger: nDigits) + low.
	lastNonZero = 0 ifFalse: [lastNonZeroHigh := lastNonZero + nDigitsHigh].
	nDigitsHigh := nDigitsHigh + nDigits.
	low isLarge]
		whileTrue: [nPackets := nPackets * 2].

	nDigits := nDigitsHigh.
	lastNonZero := lastNonZeroHigh.
	^high
]

{ #category : 'initialization' }
NumberParser >> on: aStringOrStream [
	sourceStream := aStringOrStream isString
		ifTrue: [ aStringOrStream readStream ]
		ifFalse: [ aStringOrStream ].
	base := 10.
	neg := false.
	integerPart := fractionPart := exponent := scale := 0.
	failBlock := nil
]

{ #category : 'private - parsing' }
NumberParser >> peekSignIsMinus [
	"Peek an optional sign from sourceStream.
	 Answer true if it is minus sign"

	| isMinus |
	isMinus := sourceStream peekFor: $-.
	isMinus ifFalse: [self allowPlusSign ifTrue: [sourceStream peekFor: $+]].
	^isMinus
]

{ #category : 'private - parsing' }
NumberParser >> readExponent [
	"Read the exponent if any (stored in instVar).
	 Answer true if found, answer false if none.
	 If exponent letter is not followed by a digit, this is not considered as an error.
  	 Exponent are always read in base 10."

	| eneg epos |
	exponent := 0.
	sourceStream atEnd ifTrue: [ ^ false ].
	(self exponentLetters includes: sourceStream peek) ifFalse: [ ^ false ].
	sourceStream next.
	eneg := sourceStream peekFor: $-.
	epos := eneg not
		and: [ self allowPlusSignInExponent and: [ sourceStream peekFor: $+ ] ].
	exponent := self nextUnsignedIntegerOrNilBase: 10.
		
	exponent
		ifNil: [
			"Oops, there was no digit after the exponent letter.Ungobble the letter"
			exponent := 0.
			sourceStream skip: ((eneg or: [ epos ]) ifTrue: [ -2 ] ifFalse: [ -1 ]).
			^ false ].
		
	"If the exponent is bigger than our max exponent, we return an error"
	exponent > self maxExponent
		ifTrue: [ 
				self expected: 'Exponent is larger than ' , self maxExponent printString , ' (Check Settings)'.
				exponent := 0.
				^ false ].
		
	eneg ifTrue: [ exponent := exponent negated ].
	^ true
]

{ #category : 'private - parsing' }
NumberParser >> readNamedFloatOrFail [
	"This method is used when there is no digit encountered:
	It try and read a named Float NaN or Infinity.
	Negative sign for -Infinity has been read before sending this method, and is indicated in the neg inst.var.
	Fail if no named Float is found"

	neg ifFalse: [(sourceStream nextMatchAll: 'NaN')
			ifTrue: [^ Float nan]].
	(sourceStream nextMatchAll: 'Infinity')
		ifTrue: [^ neg
			ifTrue: [Float infinity negated]
			ifFalse: [Float infinity]].
	^self expected: 'a digit between 0 and ' , (String with: (Character digitValue: base - 1))
]

{ #category : 'private - parsing' }
NumberParser >> readNumberWithFractionPartNumberOfTrailingZeroInIntegerPart: numberOfTrailingZeroInIntegerPart [
	"At this stage, sign integerPart and a decimal point have been read.
	try and form a number with a fractionPart"

	| numberOfNonZeroFractionDigits numberOfTrailingZeroInFractionPart mantissa value |
	fractionPart := self nextUnsignedIntegerOrNilBase: base.
	fractionPart ifNil: ["No fractionPart found,ungobble the decimal point and return the integerPart"
					sourceStream skip: -1.
					^ neg
						ifTrue: [integerPart negated]
						ifFalse: [integerPart]].
	numberOfNonZeroFractionDigits := lastNonZero.
	numberOfTrailingZeroInFractionPart := nDigits - lastNonZero.
	self readExponent
		ifFalse: [(self readScaleWithDefaultNumberOfDigits: nDigits)
				ifTrue: [^self makeScaledDecimalWithNumberOfNonZeroFractionDigits: numberOfNonZeroFractionDigits
					andNumberOfTrailingZeroInFractionPart: numberOfTrailingZeroInFractionPart]].

	fractionPart isZero
		ifTrue: [mantissa := integerPart
						// (base raisedToInteger: numberOfTrailingZeroInIntegerPart).
			exponent := exponent + numberOfTrailingZeroInIntegerPart]
		ifFalse: [mantissa := integerPart
						* (base raisedToInteger: numberOfNonZeroFractionDigits) + (fractionPart // (base raisedToInteger: numberOfTrailingZeroInFractionPart)).
			exponent := exponent - numberOfNonZeroFractionDigits].

	value := self makeFloatFromMantissa: mantissa exponent: exponent base: base.
	^ neg
		ifTrue: [value isZero
				ifTrue: [Float negativeZero]
				ifFalse: [value negated]]
		ifFalse: [value]
]

{ #category : 'private - parsing' }
NumberParser >> readScaleWithDefaultNumberOfDigits: anInteger [
	"Read the scale if any and store it into scale instance Variable.
	Answer true if found, answer false if none.
	The scale is specified by letter s, optionnally followed by a positive integer in base 10.
	If no integer is specified, that means using as many digits as provided after the fraction separator, as provided by parameter anInteger.
	A letter s followed by another letter is not considered as a scale specification, because it could be part of a message."

	scale := 0.
	sourceStream atEnd ifTrue: [ ^ false ].
	(sourceStream peekFor: $s) ifFalse: [ ^ false ].
	scale := self nextUnsignedIntegerOrNilBase: 10.
	scale
		ifNil: [ scale := anInteger.
			^ (sourceStream peek
				ifNil: [ false ]
				ifNotNil: [ :nextChar | nextChar isLetter ])
				ifTrue: [ sourceStream skip: -1.	"ungobble the s"
					false ]
				ifFalse: [ true ] ].
	^ true
]
