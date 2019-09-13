"
SUnit tests for large negative integers
"
Class {
	#name : #LargeNegativeIntegerTest,
	#superclass : #ClassTestCase,
	#category : #'Kernel-Tests-Numbers'
}

{ #category : #tests }
LargeNegativeIntegerTest >> testByteAt [

	| lni |
	lni := -114605103402541699037609980192546360895434064385.
	1 to: 20 do: [:i | | digit |
		digit := lni byteAt: i.
		self assert: i equals: digit]

]

{ #category : #tests }
LargeNegativeIntegerTest >> testDenormalizedPrintString [
	"Check that an un-normalized instance behaves reasonably."

	| i i0 |
	i := LargeNegativeInteger new: 4.
	i basicAt: 2 put: 255.
	self assert: i size equals: 4.
	self assert: i printString equals: '-65280'.
	self assert: i normalize equals: -65280.
	self assert: (i normalize isMemberOf: SmallInteger).
	i0 := LargeNegativeInteger new: 0.
	self assert: i0 size equals: 0.
	self assert: i0 printString equals: '-0'.
	self assert: i0 normalize equals: 0.
	self assert: (i0 normalize isMemberOf: SmallInteger)
]

{ #category : #tests }
LargeNegativeIntegerTest >> testDigitAtPut [

	| lni |
	lni := LargeNegativeInteger new: 20.
	1 to: 20 do: [:i | lni byteAt: i put: i].
	self assert: -114605103402541699037609980192546360895434064385 equals: lni

]

{ #category : #tests }
LargeNegativeIntegerTest >> testDigitLength [

	| lni |
	lni := -114605103402541699037609980192546360895434064385.
	self assert: 20 equals: lni bytesCount

]

{ #category : #tests }
LargeNegativeIntegerTest >> testEmptyTemplate [
	"Check that an uninitialized instance behaves reasonably."

	| i |
	i := LargeNegativeInteger new: 4.
	self assert: i size equals: 4.
	self assert: i printString equals: '-0'.
	self assert: i normalize equals: 0
]

{ #category : #tests }
LargeNegativeIntegerTest >> testMinimumNegativeIntegerArithmetic [
	"We are speaking of minimum integer in underlying hardware here.
	In 2-complement, abs(INT_MIN) = (INT-MAX+1) and thus overflows hardware register.
	Since some old VM forgot this edge case they may fail and it's better to be aware of it.
	http://code.google.com/p/cog/issues/detail?id=92
	http://bugs.squeak.org/view.php?id=7705
	We only test the cases of 32 and 64 bit signed integers."

	#(32 64) do: [:nBits |
		| largePositiveInt largeNegativeInt |
		largePositiveInt := (1 << (nBits - 1)).
		largeNegativeInt := largePositiveInt negated.
		self assert: (largeNegativeInt >> 3) equals: (largeNegativeInt bitInvert >> 3) bitInvert.
		self assert: (largeNegativeInt + 1) equals: (largePositiveInt - 1) negated.
		self assert: (largeNegativeInt - -1) equals: (largePositiveInt - 1) negated.
		self assert: (largeNegativeInt // -1) equals: largePositiveInt.
		self assert: (largeNegativeInt \\ -1) equals: 0.
		self assert: (largeNegativeInt rem: -1) equals: 0.
		self assert: (largeNegativeInt quo: -1) equals: largePositiveInt.
		self assert: (largeNegativeInt * -1) equals: largePositiveInt.
		self assert: (largeNegativeInt / -1) equals: largePositiveInt]
]

{ #category : #tests }
LargeNegativeIntegerTest >> testReplaceFromToWithStartingAt [

	| lni20 lni7 |
	lni20 := LargeNegativeInteger new: 20.
	1 to: 20 do: [:i | lni20 byteAt: i put: i].
	lni7 := LargeNegativeInteger new: 7.
	1 to: 7 do: [:i | lni7 byteAt: i put: 11 - i].
	lni20 replaceFrom: 6 to: 10 with: lni7 startingAt: 2.
	"unmodified digits"
	(1 to: 5) , (11 to: 20) do: [:e | | digit |
		digit := lni20 byteAt: e.
		self assert: e equals: digit].
	"replaced digits"
	6 to: 10 do: [:e | | digit replacementDigit |
		digit := lni20 byteAt: e.
		replacementDigit := lni7 byteAt: e - 4.
		self assert: replacementDigit equals: digit]

]

{ #category : #tests }
LargeNegativeIntegerTest >> testSqrt [
	self should: [(SmallInteger minVal - 1) sqrt] raise: DomainError
]

{ #category : #'tests-printing' }
LargeNegativeIntegerTest >> testStoreOn [
	| integer |
	integer := SmallInteger minVal - 1.
	
	"Store on adds a space before a negative number to avoid mistaking it with a - message"
	self
		assert: integer class equals: LargeNegativeInteger;
		assert: (String streamContents: [ :s | integer storeOn: s ]) equals: ' ', integer asString
]
