Extension { #name : 'Integer' }

{ #category : '*System-NumberPrinting' }
Integer >> asHexDigit [
	^'0123456789ABCDEF' at: self+1
]

{ #category : '*System-NumberPrinting' }
Integer >> asStringWithCommas [
	"123456789 asStringWithCommas"
	"-123456789 asStringWithCommas"
	^ String streamContents: [:stream |
		self printWithCommasOn: stream ]
]

{ #category : '*System-NumberPrinting' }
Integer >> asStringWithCommasSigned [
	"123456789 asStringWithCommasSigned"
	"-123456789 asStringWithCommasSigned"
	^ String streamContents: [:stream |
		self printWithCommasSignedOn: stream ]
]

{ #category : '*System-NumberPrinting' }
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

	"12 hex >>> '16rC'"
	"10 hex >>> '16rA'"
	"-10 hex >>> '-16rA'"

	^ self storeStringBase: 16
]

{ #category : '*System-NumberPrinting' }
Integer >> hexString [
	"Returns a string representation of the receiver as hexadecimal, prefixed with 16r."

	"Consider using any of
		printStringHex
		printStringBase: 16
		printStringBase: 16 length: 8 padded: true
		storeStringHex
		storeStringBase: 16
		storeStringBase: 16 length: 11 padded: true"

	"12 hexString >>> '16rC'"
	"10 hexString >>> '16rA'"
	"-10 hexString >>> '-16rA'"

	^ self storeStringBase: 16
]

{ #category : '*System-NumberPrinting' }
Integer >> humanReadableSISizeOn: aStream [
	"Print a SI representation of myself on the argument. See humanReadableSIByteSize for better comment."

	| exponent base |
	base := 1000.
	self < base
		ifTrue: [ ^ aStream print: self; space; nextPut: $B ].
	exponent := (self log / base log) asInteger.
	(self / (base ** exponent)) printOn: aStream showingDecimalPlaces: 2.
	aStream
		space;
		nextPut: ('kMGTPE' at: exponent);
		nextPut: $B
]

{ #category : '*System-NumberPrinting' }
Integer >> humanReadableSISizeString [
	"Return the receiver as a string with SI binary (International System of Units) file size, e.g. '50 KB'. It means that it takes 1000 and not 1024 as unit as humanReadableByteSizeString does."

	"(1000 * 1000 * 1000) humanReadableSISizeString >>> '1.00 GB'"
	"(1000 * 1000 * 1000) humanReadableByteSizeString >>> '953.67 MB'"
	"(1024 * 1024 * 1024) humanReadableSISizeString >>> '1.07 GB'"


	^ String streamContents: [ :s|
		self humanReadableSISizeOn: s ]
]

{ #category : '*System-NumberPrinting' }
Integer >> numberOfDigits [
	"Return how many digits are necessary to print this number in base 10.
	This does not count any place for minus sign, radix prefix or whatever."

	^ self numberOfDigitsInBase: 10
]

{ #category : '*System-NumberPrinting' }
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

{ #category : '*System-NumberPrinting' }
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

{ #category : '*System-NumberPrinting' }
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

{ #category : '*System-NumberPrinting' }
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

{ #category : '*System-NumberPrinting' }
Integer >> printOn: aStream base: b nDigits: n [
	"Append a representation of this number in base b on aStream using nDigits.
	self must be positive."

	self subclassResponsibility
]

{ #category : '*System-NumberPrinting' }
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

{ #category : '*System-NumberPrinting' }
Integer >> printOn: aStream showingDecimalPlaces: placesDesired [
	"Same as super, but provides a faster implementation because fraction part and rounding are trivial."

	self printOn: aStream base: 10.

	placesDesired <= 0 ifTrue: [ ^ self ].

	aStream nextPut: $..
	placesDesired timesRepeat: [ aStream nextPut: $0 ]
]

{ #category : '*System-NumberPrinting' }
Integer >> printPaddedWith: aCharacter to: anInteger [
	"Answer the string containing the ASCII representation of the receiver
	padded on the left with aCharacter to be at least anInteger characters."

	^ self
		printPaddedWith: aCharacter
		to: anInteger
		base: 10
]

{ #category : '*System-NumberPrinting' }
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

{ #category : '*System-NumberPrinting' }
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

{ #category : '*System-NumberPrinting' }
Integer >> printSeparatedBy: aDelimiter every: offset signed: printSigned on: aStream [
	^ self printSeparatedBy: aDelimiter every: offset signed: printSigned base: 10 on: aStream
]

{ #category : '*System-NumberPrinting' }
Integer >> printStringBase: base length: minimum padded: zeroFlag [

	^ String streamContents: [:s| self printOn: s base: base length: minimum padded: zeroFlag]
]

{ #category : '*System-NumberPrinting' }
Integer >> printStringHex [
	"Returns the hex digit part of the integer when printed in hexadecimal format."
	"30 printStringHex >>> '1E'"
	"30 hex >>> '16r1E'"

	^ self printStringBase: 16
]

{ #category : '*System-NumberPrinting' }
Integer >> printStringLength: minimal [
	^self printStringLength: minimal padded: false
]

{ #category : '*System-NumberPrinting' }
Integer >> printStringLength: minimal padded: zeroFlag [
	^self printStringBase: 10 length: minimal padded: zeroFlag
]

{ #category : '*System-NumberPrinting' }
Integer >> printStringPadded: minimal [
	^self printStringLength: minimal padded: true
]

{ #category : '*System-NumberPrinting' }
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
			^ self negative
				ifTrue: [ '-10r' , tempString allButFirst ]
				ifFalse: [ '10r' , tempString ]].

	^ self storeStringBase: baseInteger
]

{ #category : '*System-NumberPrinting' }
Integer >> printWithCommasOn: aStream [
	"123456789 asStringWithCommas"
	"-123456789 asStringWithCommas"
	^ self printSeparatedBy: $, every: 3 signed: false on: aStream
]

{ #category : '*System-NumberPrinting' }
Integer >> printWithCommasSignedOn: aStream [
	"123456789 asStringWithCommasSigned"
	"-123456789 asStringWithCommasSigned"
	^ self printSeparatedBy: $, every: 3 signed: true on: aStream
]

{ #category : '*System-NumberPrinting' }
Integer >> radix: base [
	^ self printStringBase: base
]

{ #category : '*System-NumberPrinting' }
Integer >> storeOn: aStream [
	self < 0
		ifTrue: [ aStream space ].

	super storeOn: aStream
]

{ #category : '*System-NumberPrinting' }
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
	aStream nextPutAll: (integer printStringBase: base)
]

{ #category : '*System-NumberPrinting' }
Integer >> storeOn: aStream base: base length: minimum padded: zeroFlag [
	| prefix |
	prefix := self negative ifTrue: ['-'] ifFalse: [String new].
	base = 10 ifFalse: [prefix := prefix, base printString, 'r'].
	self print: (self abs printStringBase: base) on: aStream prefix: prefix length: minimum padded: zeroFlag
]

{ #category : '*System-NumberPrinting' }
Integer >> storeStringBase: base length: minimum padded: zeroFlag [
	"(10 storeStringBase: 16 length: 6 padded: true) >>> '16r00A'"

	^ String streamContents: [:s|
			self storeOn: s base: base length: minimum padded: zeroFlag ]
]

{ #category : '*System-NumberPrinting' }
Integer >> storeStringHex [
	"Returns a string representation of the receiver in base 16 in a way that executing it will return the receiver."
	"10 storeStringHex >>> '16rA'"

	^ self storeStringBase: 16
]
