Extension { #name : 'Float' }

{ #category : '*System-NumberPrinting' }
Float >> absPrintExactlyOn: aStream base: base [
	"Print my value on a stream in the given base.  Assumes that my value is strictly
	positive; negative numbers, zero, NaNs and infinite numbers have already been handled elsewhere.
	Based upon the algorithm outlined in:
	Robert G. Burger and R. Kent Dybvig
	Printing Floating Point Numbers Quickly and Accurately
	ACM SIGPLAN 1996 Conference on Programming Language Design and Implementation
	June 1996.
	This version guarantees that the printed representation exactly represents my value
	by using exact integer arithmetic."

	| fBase significand exp baseExpEstimate be be1 r s mPlus mMinus scale roundingIncludesLimits d tc1 tc2 fixedFormat decPointCount |
	fBase := base asFloat.
	significand := self significandAsInteger.
	roundingIncludesLimits := significand even.
	exp := (self exponent - 52) max: MinValLogBase2.
	baseExpEstimate := (self exponent * fBase reciprocalLogBase2 - 1.0e-10) ceiling.
	exp >= 0
		ifTrue:
			[be := 1 << exp.
			significand ~= 16r10000000000000
				ifTrue:
					[r := significand * be * 2.
					s := 2.
					mPlus := be]
				ifFalse:
					[be1 := be * 2.
					r := significand * be1 * 2.
					s := 4.
					mPlus := be1].
				mMinus := be]
		ifFalse:
			[(exp = MinValLogBase2) | (significand ~= 16r10000000000000)
				ifTrue:
					[r := significand * 2.
					s := (1 << (exp negated)) * 2.
					mPlus := 1]
				ifFalse:
					[r := significand * 4.
					s := (1 << (exp negated + 1)) * 2.
					mPlus := 2].
				mMinus := 1].
	baseExpEstimate >= 0
		ifTrue: [s := s * (base raisedToInteger: baseExpEstimate)]
		ifFalse:
			[scale := base raisedToInteger: baseExpEstimate negated.
			r := r * scale.
			mPlus := mPlus * scale.
			mMinus := mMinus * scale].
	(r + mPlus > s) | (roundingIncludesLimits & (r + mPlus = s))
		ifTrue: [baseExpEstimate := baseExpEstimate + 1]
		ifFalse:
			[r := r * base.
			mPlus := mPlus * base.
			mMinus := mMinus * base].
	(fixedFormat := baseExpEstimate between: -3 and: 6)
		ifTrue:
			[decPointCount := baseExpEstimate.
			baseExpEstimate <= 0
				ifTrue: [aStream nextPutAll: ('0.000000' truncateTo: 2 - baseExpEstimate)]]
		ifFalse:
			[decPointCount := 1].
	[d := r // s.
	r := r \\ s.
	(tc1 := (r < mMinus) | (roundingIncludesLimits & (r = mMinus))) |
	(tc2 := (r + mPlus > s) | (roundingIncludesLimits & (r + mPlus = s)))] whileFalse:
		[aStream nextPut: (Character digitValue: d).
		r := r * base.
		mPlus := mPlus * base.
		mMinus := mMinus * base.
		decPointCount := decPointCount - 1.
		decPointCount = 0 ifTrue: [aStream nextPut: $.]].
	tc2 ifTrue:
		[tc1 not | (tc1 & (r*2 >= s)) ifTrue: [d := d + 1]].
	aStream nextPut: (Character digitValue: d).
	decPointCount > 0
		ifTrue:
		[decPointCount - 1 to: 1 by: -1 do: [:i | aStream nextPut: $0].
		aStream nextPutAll: '.0'].
	fixedFormat ifFalse:
		[aStream nextPut: $e.
		aStream nextPutAll: (baseExpEstimate - 1) printString]
]

{ #category : '*System-NumberPrinting' }
Float >> absPrintInexactlyOn: aStream base: base [
	"Print my value on a stream in the given base.  Assumes that my value is strictly
	positive; negative numbers, zero, NaNs and infinite numbers have already been handled elsewhere.
	Based upon the algorithm outlined in:
	Robert G. Burger and R. Kent Dybvig
	Printing Floating Point Numbers Quickly and Accurately
	ACM SIGPLAN 1996 Conference on Programming Language Design and Implementation
	June 1996.
	This version performs all calculations with Floats instead of LargeIntegers, and loses
	about 3 lsbs of accuracy compared to an exact conversion."

	| significantBits fBase exp baseExpEstimate r s mPlus mMinus scale d tc1 tc2 fixedFormat decPointCount |
	significantBits := 50.  "approximately 3 lsb's of accuracy loss during conversion"
	fBase := base asFloat.
	exp := self exponent.
	baseExpEstimate := (exp * fBase reciprocalLogBase2 - 1.0e-10) ceiling.
	exp >= 0
		ifTrue:
			[r := self.
			s := 1.0.
			mPlus := 1.0 timesTwoPower: exp - significantBits.
			mMinus := self significand ~= 1.0 ifTrue: [mPlus] ifFalse: [mPlus / 2.0]]
		ifFalse:
			[r := self timesTwoPower: significantBits.
			s := 1.0 timesTwoPower:  significantBits.
			mMinus := 1.0 timesTwoPower: (exp max: -1024).
			mPlus :=
				(exp = MinValLogBase2) | (self significand ~= 1.0)
					ifTrue: [mMinus]
					ifFalse: [mMinus * 2.0]].
	baseExpEstimate >= 0
		ifTrue:
			[s := s * (fBase raisedToInteger: baseExpEstimate).
			exp = 1023
				ifTrue:   "scale down to prevent overflow to Infinity during conversion"
					[r := r / fBase.
					s := s / fBase.
					mPlus := mPlus / fBase.
					mMinus := mMinus / fBase]]
		ifFalse:
			[exp < -1023
				ifTrue:   "scale up to prevent denorm reciprocals overflowing to Infinity"
					[d := (53 * fBase reciprocalLogBase2 - 1.0e-10) ceiling.
					scale := fBase raisedToInteger: d.
					r := r * scale.
					mPlus := mPlus * scale.
					mMinus := mMinus * scale.
					scale := fBase raisedToInteger: (baseExpEstimate + d) negated]
				ifFalse:
				[scale := fBase raisedToInteger: baseExpEstimate negated].
			s := s / scale].
	(r + mPlus >= s)
		ifTrue: [baseExpEstimate := baseExpEstimate + 1]
		ifFalse:
			[s := s / fBase].
	(fixedFormat := baseExpEstimate between: -3 and: 6)
		ifTrue:
			[decPointCount := baseExpEstimate.
			baseExpEstimate <= 0
				ifTrue: [aStream nextPutAll: ('0.000000' truncateTo: 2 - baseExpEstimate)]]
		ifFalse:
			[decPointCount := 1].
	[d := (r / s) truncated.
	r := r - (d * s).
	(tc1 := r <= mMinus) | (tc2 := r + mPlus >= s)] whileFalse:
		[aStream nextPut: (Character digitValue: d).
		r := r * fBase.
		mPlus := mPlus * fBase.
		mMinus := mMinus * fBase.
		decPointCount := decPointCount - 1.
		decPointCount = 0 ifTrue: [aStream nextPut: $.]].
	tc2 ifTrue:
		[tc1 not | (tc1 & (r*2.0 >= s)) ifTrue: [d := d + 1]].
	aStream nextPut: (Character digitValue: d).
	decPointCount > 0
		ifTrue:
		[decPointCount - 1 to: 1 by: -1 do: [:i | aStream nextPut: $0].
		aStream nextPutAll: '.0'].
	fixedFormat ifFalse:
		[aStream nextPut: $e.
		aStream nextPutAll: (baseExpEstimate - 1) printString]
]

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
Float >> printOn: stream base: base [
	"Handle sign, zero, and NaNs; all other values passed to FloatPrintPolicy"

	self isNaN ifTrue: [ ^ stream nextPutAll: 'Float nan' ]. "check for NaN before sign"
	self isInfinite
		ifTrue: [
			stream nextPutAll: 'Float infinity'.
			^ self sign = -1 ifTrue: [ stream nextPutAll: ' negated' ] ].
	self > 0.0
		ifTrue: [ FloatPrintPolicy absPrint: self on: stream base: base ]
		ifFalse: [
			self signBit = 1 ifTrue: [ stream nextPut: $- ].
			self = 0.0
				ifTrue: [ stream nextPutAll: '0.0' ]
				ifFalse: [ FloatPrintPolicy absPrint: self negated on: stream base: base ] ]
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
