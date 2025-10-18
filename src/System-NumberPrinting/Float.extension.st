Extension { #name : 'Float' }

{ #category : '*System-NumberPrinting' }
Float >> binaryLiteralString [

	^ String streamContents: [ :stream | self printBinaryLiteralOn: stream ]
]

{ #category : '*System-NumberPrinting' }
Float >> hex [

	^ String streamContents:
		[:strm | | word nibble |
		1 to: 2 do:
			[:i | word := self at: i.
			1 to: 8 do:
				[:s | nibble := (word bitShift: -8+s*4) bitAnd: 16rF.
				strm nextPut: ('0123456789ABCDEF' at: nibble+1)]]]
"
(-2.0 to: 2.0) collect: [:f | f hex]
"
]

{ #category : '*System-NumberPrinting' }
Float >> printBinaryLiteralOn: stream [

	"Floats represent those numbers that can be written in Pharo notation as literals of the form {sign}2r1.{fraction}e{exponent} -- where {sign} is the minus
	sign or is empty, {fraction} is a sequence of 52 bits and {exponent} is an integer from -1022 to 1023 -- as well as those numbers that can be written
	as {sign}2r0.{fraction}e-1022 and some special values. This method prints a Float as such a literal expression, or a message expression in the case of
	the special values. Evaluating the expression always results in a Float with the exact same bit pattern, with the exception of the not-a-number Floats
	which all get printed as 'Float nan'. The following article has a table in the section 'Summary' at the end which enumerates the different bit patterns
	of Floats and how to interpret them, which was used in writing this method: http://steve.hollasch.net/cgindex/coding/ieeefloat.html"

	| signBit exponentBits fractionBits |

	signBit := (self basicAt: 1) bitShift: -31.
	exponentBits := ((self basicAt: 1) bitShift: -20) bitAnd: 2r11111111111.
	fractionBits := (((self basicAt: 1) bitAnd: 2r11111111111111111111) bitShift: 32) bitOr: (self basicAt: 2).

	(exponentBits = 2047) ifTrue: [
		(fractionBits = 0) ifTrue: [
			stream nextPutAll: 'Float infinity'.
			(signBit = 1) ifTrue: [ stream nextPutAll: ' negated' ].
		] ifFalse: [
			stream nextPutAll: 'Float nan'.
		].
		^ self
	].

	(signBit = 1) ifTrue: [ stream nextPut: $- ].
	(exponentBits = 0) ifTrue: [
		(fractionBits = 0) ifTrue: [
			stream nextPutAll: '0.0'
		] ifFalse: [
			stream nextPutAll: '2r0.'.
			fractionBits printOn: stream base: 2 length: (Float precision - 1) padded: true.
			stream nextPutAll: 'e-1022'.
		].
		^ self
	].

	stream nextPutAll: '2r1.'.
	fractionBits printOn: stream base: 2 length: (Float precision - 1) padded: true.
	stream nextPut: $e.
	(exponentBits - 1023) printOn: stream base: 10
]

{ #category : '*System-NumberPrinting' }
Float >> printPaddedWith: aCharacter to: aNumber [
	"Answer the string containing the ASCII representation of the receiver
	padded on the left with aCharacter to be at least on aNumber
	integerPart characters and padded the right with aCharacter to be at
	least anInteger fractionPart characters."
	| aStream digits fPadding fLen iPadding iLen curLen periodIndex |

	"2000/03/04  Harmon R. Added Date and Time support"
	aStream := (String new: 10) writeStream.
	self printOn: aStream.
	digits := aStream contents.
	periodIndex := digits indexOf: $..
	curLen := periodIndex - 1.
	iLen := aNumber integerPart.
	curLen < iLen
		ifTrue: [iPadding := (String new: (iLen - curLen) asInteger) atAllPut: aCharacter;
					 yourself]
		ifFalse: [iPadding := ''].
	curLen := digits size - periodIndex.
	"n.b. Treat aNumber as a string format specifier rather than as a number, because
	floating point truncation can produce incorrect results for the fraction part."
	fLen := (aNumber asString copyAfterLast: $. )
		ifNotEmpty: [:s | s asInteger]
		ifEmpty: [ 0 ].
	curLen < fLen
		ifTrue: [fPadding := (String new: fLen - curLen) atAllPut: aCharacter;
					 yourself]
		ifFalse: [fPadding := ''].
	^ iPadding , digits , fPadding
]

{ #category : '*System-NumberPrinting' }
Float >> printShowingDecimalPlaces: placesDesired [
	"This implementation avoids any rounding error caused by rounded or roundTo:"

	^self asTrueFraction printShowingDecimalPlaces: placesDesired
]

{ #category : '*System-NumberPrinting' }
Float >> storeOn: aStream [
	"Print the Number exactly so it can be interpreted back unchanged"

	self storeOn: aStream base: 10
]

{ #category : '*System-NumberPrinting' }
Float >> storeOn: aStream base: base [
	"Print the Number exactly so it can be interpreted back unchanged"
	self isFinite
		ifTrue: [self signBit = 1 ifTrue: [aStream nextPutAll: ' -'].
			base = 10 ifFalse: [aStream print: base; nextPut: $r].
			self = 0.0
				ifTrue: [aStream nextPutAll: '0.0']
				ifFalse: [self abs absPrintExactlyOn: aStream base: base]]
		ifFalse: [self isNaN
				ifTrue: [aStream nextPutAll: 'Float nan']
				ifFalse: [self > 0.0
						ifTrue: [aStream nextPutAll: 'Float infinity']
						ifFalse: [aStream nextPutAll: 'Float infinity negated']]]
]
