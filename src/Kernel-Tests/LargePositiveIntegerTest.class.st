"
SUnit tests for large positive integers
"
Class {
	#name : #LargePositiveIntegerTest,
	#superclass : #ClassTestCase,
	#category : #'Kernel-Tests-Numbers'
}

{ #category : #tests }
LargePositiveIntegerTest >> assertSqrtCorrectlyRoundedForExponent: exp [
	"Requires exp > Float precision, so that f ulp/2 is integer"
	{1.5. 1.25 squared. 2.0 predecessor} do: [:sf |
		| f xe xp xm |
		
		f := sf timesTwoPower: exp.
	
		"make two integers around the pivot"
		xe := f asInteger + (f ulp asInteger / 2).
		xm := xe squared - 1.
		xp := xe squared + 1.
		self assert: xe squared sqrt equals: xe.
		self assert: xe squared sqrt isInteger.
	
		"check rounding when result is near f squared"
		self assert: xm sqrt equals: f.
		self assert: xm sqrt isFloat.
		self assert: xp sqrt equals: f successor.
		self assert: xp sqrt isFloat.
	
		"same in the other direction"
		xe := f asInteger - (f ulp asInteger / 2).
		xm := xe squared - 1.
		xp := xe squared + 1.
		self assert: xe squared sqrt equals: xe.
		self assert: xe squared sqrt isInteger.
	
		"check rounding when result is near f squared"
		self assert: xm sqrt equals: f predecessor.
		self assert: xm sqrt isFloat.
		self assert: xp sqrt equals: f.
		self assert: xp sqrt isFloat].
]

{ #category : #tests }
LargePositiveIntegerTest >> testBitShift [

	"Check bitShift from and back to SmallInts"
	
	1 to: 257 do: [:i | self should: [((i bitShift: i) bitShift: 0-i) == i]].
]

{ #category : #tests }
LargePositiveIntegerTest >> testDenormalizedPrintString [
	"Check that an un-normalized instance behaves reasonably."

	| i i0 |
	i := LargePositiveInteger new: 4.
	i basicAt: 2 put: 255.
	self assert: i size equals: 4.
	self assert: i printString equals: '65280'.
	self assert: i normalize equals: 65280.
	self assert: (i normalize isMemberOf: SmallInteger).
	i0 := LargePositiveInteger new: 0.
	self assert: i0 size equals: 0.
	self assert: i0 printString equals: '0'.
	self assert: i0 normalize equals: 0.
	self assert: (i0 normalize isMemberOf: SmallInteger)
]

{ #category : #tests }
LargePositiveIntegerTest >> testDigitAt [

	| lpi |
	lpi := 114605103402541699037609980192546360895434064385.
	1 to: 20 do: [:i | | digit |
		digit := lpi byteAt: i.
		self assert: i equals: digit]

]

{ #category : #tests }
LargePositiveIntegerTest >> testDigitAtPut [

	| lpi |
	lpi := LargePositiveInteger new: 20.
	1 to: 20 do: [:i | lpi byteAt: i put: i].
	self assert: 114605103402541699037609980192546360895434064385 equals: lpi

]

{ #category : #tests }
LargePositiveIntegerTest >> testDigitLength [

	| lpi |
	lpi := 114605103402541699037609980192546360895434064385.
	self assert: 20 equals: lpi bytesCount

]

{ #category : #tests }
LargePositiveIntegerTest >> testEmptyTemplate [
	"Check that an uninitialized instance behaves reasonably."

	| i |
	i := LargePositiveInteger new: 4.
	self assert: i size equals: 4.
	self assert: i printString equals: '0'.
	self assert: i normalize equals: 0
]

{ #category : #tests }
LargePositiveIntegerTest >> testNormalize [
	"Check normalization and conversion to/from SmallInts"

	self assert: SmallInteger maxVal + 1 - 1 identicalTo: SmallInteger maxVal.
	self assert: SmallInteger maxVal + 3 - 6 identicalTo: SmallInteger maxVal - 3.
	self should: SmallInteger minVal - 1 + 1 == SmallInteger minVal.
	self assert: SmallInteger minVal - 3 + 6 identicalTo: SmallInteger minVal + 3
]

{ #category : #tests }
LargePositiveIntegerTest >> testReciprocalModulo [
	| large r |
	large := 1 bitShift: 48.
	r := Random seed: 46912151.
	4691
		timesRepeat: [ | a b c t |
			a := (r nextInt: large) + 1.
			b := (r nextInt: large) + 1.
			a > b
				ifTrue: [ t := a.
					a := b.
					b := t ].
			(a gcd: b) = 1
				ifTrue: [ c := a reciprocalModulo: b.
					self assert: a * c \\ b equals: 1 ]
				ifFalse: [ self should: [ a reciprocalModulo: b ] raise: Error ] ]
]

{ #category : #tests }
LargePositiveIntegerTest >> testReplaceFromToWithStartingAt [

	| lpi20 lpi7 |
	lpi20 := LargePositiveInteger new: 20.
	1 to: 20 do: [:i | lpi20 byteAt: i put: i].
	lpi7 := LargePositiveInteger new: 7.
	1 to: 7 do: [:i | lpi7 byteAt: i put: 11 - i].
	lpi20 replaceFrom: 6 to: 10 with: lpi7 startingAt: 2.
	"unmodified digits"
	(1 to: 5) , (11 to: 20) do: [:e | | digit |
		digit := lpi20 byteAt: e.
		self assert: e equals: digit].
	"replaced digits"
	6 to: 10 do: [:e | | digit replacementDigit |
		digit := lpi20 byteAt: e.
		replacementDigit := lpi7 byteAt: e - 4.
		self assert: replacementDigit equals: digit]

]

{ #category : #tests }
LargePositiveIntegerTest >> testSqrt [
	self assert: (SmallInteger maxVal + 1) sqrt equals: (SmallInteger maxVal + 1) asFloat sqrt.
]

{ #category : #'tests-printing' }
LargePositiveIntegerTest >> testStoreOn [
	| integer |
	integer := SmallInteger maxVal + 1.
	self
		assert: integer class equals: LargePositiveInteger;
		assert: (String streamContents: [ :s | integer storeOn: s ]) equals: integer asString
]

{ #category : #tests }
LargePositiveIntegerTest >> x106kbits [
	"Return a 106 kilo bits integer"
	^(15 to: 55 by: 4)
				inject: 9876543210
				into: [:big :bits | big * big << bits + bits]
]

{ #category : #tests }
LargePositiveIntegerTest >> x13kbits [
	"Return a 13 kilo bits integer"
	^(15 to: 44 by: 4)
				inject: 9753102468
				into: [:big :bits | big * big << bits + bits]
]

{ #category : #tests }
LargePositiveIntegerTest >> x23kbits [
	"Return a 23 kilo bits integer"
	^(11 to: 44 by: 4)
			inject: 1234567890
			into: [:big :bits | big * big << bits + bits]
]

{ #category : #tests }
LargePositiveIntegerTest >> x92kbits [
	"Return a 92 kilo bits integer"
	^(11 to: 51 by: 4)
			inject: 1357924680
			into: [:big :bits | big * big << bits + bits]
]
