"
SUnit tests for fractions
"
Class {
	#name : #FractionTest,
	#superclass : #ClassTestCase,
	#category : #'Kernel-Tests-Numbers'
}

{ #category : #private }
FractionTest >> assert: a classAndValueEquals: b [
	self assert: a class = b class.
	self assert: a = b
]

{ #category : #'tests - conversions' }
FractionTest >> testAsLargerPowerOfTwo [

"SmallIntegers, Fraction value < 1"
	"Exact power of two"
	self assert: (1/2) asLargerPowerOfTwo equals: 1/2.
	"Non-reduced exact power of two"
	self assert: (Fraction numerator: 2 denominator:  4) asLargerPowerOfTwo equals: 1/2.
	"Not power of two"
	self assert: (2 / 5) asLargerPowerOfTwo equals: 1/2.
	"Non-reduced non-power of two"
	self assert: (Fraction numerator: 10 denominator: 25) asLargerPowerOfTwo equals: 1/2.
"SmallIntegers, Fraction value  > 1"
	"Exact power of two"
	self assert: (Fraction numerator: 2 denominator:  1) asLargerPowerOfTwo equals: 2.
	"Non-reduced exact power of two"
	self assert: (Fraction numerator: 4 denominator:  2) asLargerPowerOfTwo equals: 2.
	"Not power of two"
	self assert: (3 / 2) asLargerPowerOfTwo equals: 2.
	"Non-reduced non-power of two"
	self assert: (Fraction numerator: 12 denominator: 8) asLargerPowerOfTwo equals: 2.
"LargeIntegers, Fraction value < 1"
	"Exact power of two"
	self assert: (1/(2 raisedTo:80)) asLargerPowerOfTwo equals: 1/(2 raisedTo: 80).
	"Non-reduced exact power of two"
	self assert: (Fraction numerator: (2 raisedTo: 80) denominator: (2 raisedTo: 160)) asLargerPowerOfTwo equals: 1/(2 raisedTo: 80).
	"Not power of two"
	self assert: (1/((2 raisedTo: 80)+1)) asLargerPowerOfTwo equals: 1/(2 raisedTo: 80).
	"Non-reduced non-power of two"
	self assert: (Fraction numerator: (2 raisedTo: 80) denominator: (2 raisedTo: 160) + 1) asLargerPowerOfTwo equals: 1/(2 raisedTo: 80).
"LargeIntegers, Fraction value > 1"
	"Exact power of two"
	self assert: (Fraction numerator: (2 raisedTo:80) denominator:  1) asLargerPowerOfTwo equals: (2 raisedTo: 80).
	"Non-reduced exact power of two"
	self assert: (Fraction numerator: (2 raisedTo: 160) denominator: (2 raisedTo: 80)) asLargerPowerOfTwo equals: (2 raisedTo: 80).
	"Not power of two"
	self assert: (Fraction numerator: (2 raisedTo:80) -1 denominator:  1) asLargerPowerOfTwo equals: (2 raisedTo: 80).
	"Non-reduced non-power of two"
	self assert: (Fraction numerator: (2 raisedTo: 160) -1 denominator: (2 raisedTo: 80)) asLargerPowerOfTwo equals: (2 raisedTo: 80).

	
"Error condition: numerator is 0"
	self should: [(Fraction numerator: 0 denominator: 1) asLargerPowerOfTwo] raise: DomainError.
"Errro condition: Negative fraction"
	"SmallIntegers, variations of negated numerator/denominator, and reduced/ non-reduced "
	self should: [(Fraction numerator: 1 negated denominator: 2) asLargerPowerOfTwo] raise: DomainError.
	self should: [(Fraction numerator: 1 denominator: 2 negated) asLargerPowerOfTwo] raise: DomainError.
	self should: [(Fraction numerator: 2 negated denominator: 4) asLargerPowerOfTwo] raise: DomainError.
	self should: [(Fraction numerator: 2 denominator: 4 negated) asLargerPowerOfTwo] raise: DomainError.
	"LargeNegativeIntegers, variations of negated numerator/denominator, and reduced/ non-reduced "
	self should: [(Fraction numerator: 1 negated denominator: (2 raisedTo: 80)) asLargerPowerOfTwo] raise: DomainError.
	self should: [(Fraction numerator: 1 denominator:(2 raisedTo: 80) negated) asLargerPowerOfTwo] raise: DomainError.
	self should: [(Fraction numerator: (2 raisedTo: 80) negated denominator: (2 raisedTo: 160)) asLargerPowerOfTwo] raise: DomainError.
	self should: [(Fraction numerator: (2 raisedTo: 80) denominator: (2 raisedTo: 160) negated) asLargerPowerOfTwo] raise: DomainError
	

]

{ #category : #'tests - conversions' }
FractionTest >> testAsSmallerPowerOfTwo [

"SmallIntegers, Fraction value < 1"
	"Exact power of two"
	self assert: (1/2) asSmallerPowerOfTwo equals: 1/2.
	"Non-reduced exact power of two"
	self assert: (Fraction numerator: 2 denominator:  4) asSmallerPowerOfTwo equals: 1/2.
	"Not power of two"
	self assert: (2 / 5) asSmallerPowerOfTwo equals: 1/4.
	"Non-reduced non-power of two"
	self assert: (Fraction numerator: 10 denominator: 25) asSmallerPowerOfTwo equals: 1/4.
"SmallIntegers, Fraction value  > 1"
	"Exact power of two"
	self assert: (Fraction numerator: 2 denominator:  1) asSmallerPowerOfTwo equals: 2.
	"Non-reduced exact power of two"
	self assert: (Fraction numerator: 4 denominator:  2) asSmallerPowerOfTwo equals: 2.
	"Not power of two"
	self assert: (11 / 2) asSmallerPowerOfTwo equals: 4.
	"Non-reduced non-power of two"
	self assert: (Fraction numerator: 44 denominator: 8) asSmallerPowerOfTwo equals: 4.
"LargeIntegers, Fraction value < 1"
	"Exact power of two"
	self assert: (1/(2 raisedTo:80)) asSmallerPowerOfTwo equals: 1/(2 raisedTo: 80).
	"Non-reduced exact power of two"
	self assert: (Fraction numerator: (2 raisedTo: 80) denominator: (2 raisedTo: 160)) asSmallerPowerOfTwo equals: 1/(2 raisedTo: 80).
	"Not power of two"
	self assert: (1/((2 raisedTo:80) - 1)) asSmallerPowerOfTwo equals: 1/(2 raisedTo: 80).
	"Non-reduced non-power of two"
	self assert: (Fraction numerator: (2 raisedTo: 80) denominator: (2 raisedTo: 160) - 1) asSmallerPowerOfTwo equals: 1/(2 raisedTo: 80).
"LargeIntegers, Fraction value > 1"
	"Exact power of two"
	self assert: (Fraction numerator: (2 raisedTo:80) denominator:  1) asSmallerPowerOfTwo equals: (2 raisedTo: 80).
	"Non-reduced exact power of two"
	self assert: (Fraction numerator: (2 raisedTo: 160) denominator: (2 raisedTo: 80)) asSmallerPowerOfTwo equals: (2 raisedTo: 80).
	"Not power of two"
	self assert: (Fraction numerator: (2 raisedTo:80) +1 denominator:  1) asSmallerPowerOfTwo equals: (2 raisedTo: 80).
	"Non-reduced non-power of two"
	self assert: (Fraction numerator: (2 raisedTo: 160) +1 denominator: (2 raisedTo: 80)) asSmallerPowerOfTwo equals: (2 raisedTo: 80).

	
"Error condition: numerator is 0"
	self should: [(Fraction numerator: 0 denominator: 1) asSmallerPowerOfTwo] raise: DomainError.
"Errro condition: Negative fraction"
	"SmallIntegers, variations of negated numerator/denominator, and reduced/ non-reduced "
	self should: [(Fraction numerator: 1 negated denominator: 2) asSmallerPowerOfTwo] raise: DomainError.
	self should: [(Fraction numerator: 1 denominator: 2 negated) asSmallerPowerOfTwo] raise: DomainError.
	self should: [(Fraction numerator: 2 negated denominator: 4) asSmallerPowerOfTwo] raise: DomainError.
	self should: [(Fraction numerator: 2 denominator: 4 negated) asSmallerPowerOfTwo] raise: DomainError.
	"LargeNegativeIntegers, variations of negated numerator/denominator, and reduced/ non-reduced "
	self should: [(Fraction numerator: 1 negated denominator: (2 raisedTo: 80)) asSmallerPowerOfTwo] raise: DomainError.
	self should: [(Fraction numerator: 1 denominator:(2 raisedTo: 80) negated) asSmallerPowerOfTwo] raise: DomainError.
	self should: [(Fraction numerator: (2 raisedTo: 80) negated denominator: (2 raisedTo: 160)) asSmallerPowerOfTwo] raise: DomainError.
	self should: [(Fraction numerator: (2 raisedTo: 80) denominator: (2 raisedTo: 160) negated) asSmallerPowerOfTwo] raise: DomainError

]

{ #category : #'tests - conversions' }
FractionTest >> testCeiling [
	self assert: (3 / 2) ceiling = 2.
	self assert: (-3 / 2) ceiling = -1.
]

{ #category : #'tests - mathematical functions' }
FractionTest >> testDegreeCos [
	"self run: #testDegreeCos"

	(4 / 3) degreeCos.
	-361 / 3 to: 359 / 3 do: [ :i | self assert: (i degreeCos closeTo: i degreesToRadians cos) ]
]

{ #category : #'tests - mathematical functions' }
FractionTest >> testDegreeSin [
	"self run: #testDegreeSin"

	(4 / 3) degreeSin.
	-361 / 3 to: 359 / 3 do: [ :i | self assert: (i degreeSin closeTo: i degreesToRadians sin) ]
]

{ #category : #'tests - mathematical functions' }
FractionTest >> testExactRaisedTo [
	"
	FractionTest new testExactRaisedTo
	"
	| f |
	self assert: (4/9 raisedTo: 1/2) classAndValueEquals: 2/3.
	self assert: (9/4 raisedTo: 1/2) classAndValueEquals: 3/2.
	#( 1 5 29 135 1234 567890 123123123 456456456456 98765432109876543210987654321 987123987123987123987123987123987123987123987123) pairsDo: [ :a :b |
		f := a / b.
		self assert: (f squared raisedTo: 1/2) classAndValueEquals: f.
		self assert: (f negated squared raisedTo: 1/2) classAndValueEquals: f.
		f := b / a.
		self assert: (f squared raisedTo: 1/2) classAndValueEquals: f.
		self assert: (f negated squared raisedTo: 1/2) classAndValueEquals: f ].

	self assert: (8/27 raisedTo: 1/3) classAndValueEquals: 2/3.
	self assert: (27/8 raisedTo: 1/3) classAndValueEquals: 3/2.
	#( 1 5 29 135 1234 567890 123123123 456456456456 98765432109876543210987654321 987123987123987123987123987123987123987123987123) pairsDo: [ :a :b |
		f := a / b.
		self assert: ((f raisedTo: 3) raisedTo: 1/3) classAndValueEquals: f.
		self assert: ((f negated raisedTo: 3) raisedTo: 1/3) classAndValueEquals: f negated.
		f := b / a.
		self assert: ((f raisedTo: 3) raisedTo: 1/3) classAndValueEquals: f.
		self assert: ((f negated raisedTo: 3) raisedTo: 1/3) classAndValueEquals: f negated ].

	self assert: (4/9 raisedTo: 3/2) classAndValueEquals: 8/27.
	self assert: (8/27 raisedTo: 2/3) classAndValueEquals: 4/9.
	#( 1 5 29 135 1234 567890 123123123 456456456456 98765432109876543210987654321 987123987123987123987123987123987123987123987123) pairsDo: [ :a :b |
		f := a / b.
		self assert: ((f raisedTo: 3) raisedTo: 2/3) classAndValueEquals: f*f.
		self assert: ((f raisedTo: 2) raisedTo: 3/2) classAndValueEquals: f*f*f.
		self assert: ((f negated raisedTo: 3) raisedTo: 2/3) classAndValueEquals: f*f.
		self assert: ((f negated raisedTo: 2) raisedTo: 3/2) classAndValueEquals: f*f*f.
		f := b / a.
		self assert: ((f raisedTo: 3) raisedTo: 2/3) classAndValueEquals: f*f.
		self assert: ((f raisedTo: 2) raisedTo: 3/2) classAndValueEquals: f*f*f.
		self assert: ((f negated raisedTo: 3) raisedTo: 2/3) classAndValueEquals: f*f.
		self assert: ((f negated raisedTo: 2) raisedTo: 3/2) classAndValueEquals: f*f*f ].

	self assert: (32/243 raisedTo: 3/5) classAndValueEquals: 8/27.
	self assert: (8/27 raisedTo: 5/3) classAndValueEquals: 32/243.
	#( 1 5 29 135 1234 567890 123123123 456456456456 98765432109876543210987654321 987123987123987123987123987123987123987123987123) pairsDo: [ :a :b |
		f := a / b.
		self assert: ((f raisedTo: 5) raisedTo: 3/5) classAndValueEquals: f*f*f.
		self assert: ((f raisedTo: 3) raisedTo: 5/3) classAndValueEquals: f*f*f*f*f.
		self assert: ((f negated raisedTo: 5) raisedTo: 3/5) classAndValueEquals: (f*f*f) negated.
		self assert: ((f negated raisedTo: 3) raisedTo: 5/3) classAndValueEquals: (f*f*f*f*f) negated.

		self assert: ((f raisedTo: -5) raisedTo: 3/5) classAndValueEquals: 1/(f*f*f).
		self assert: ((f raisedTo: -3) raisedTo: 5/3) classAndValueEquals: 1/(f*f*f*f*f).
		self assert: ((f negated raisedTo: -5) raisedTo: 3/5) classAndValueEquals: -1/(f*f*f).
		self assert: ((f negated raisedTo: -3) raisedTo: 5/3) classAndValueEquals: -1/(f*f*f*f*f).
		self assert: ((f raisedTo: 5) raisedTo: -3/5) classAndValueEquals: 1/(f*f*f).
		self assert: ((f raisedTo: 3) raisedTo: -5/3) classAndValueEquals: 1/(f*f*f*f*f).
		self assert: ((f negated raisedTo: 5) raisedTo: -3/5) classAndValueEquals: -1/(f*f*f).
		self assert: ((f negated raisedTo: 3) raisedTo: -5/3) classAndValueEquals: -1/(f*f*f*f*f).

		"No exact result => Float result"
		self assert: ((f raisedTo: 3) +1 raisedTo: 5/3) isFloat.
		self assert: ((f negated raisedTo: 3) -1 raisedTo: 5/3) isFloat.

		f := b / a.
		self assert: ((f raisedTo: 5) raisedTo: 3/5) classAndValueEquals: f*f*f.
		self assert: ((f raisedTo: 3) raisedTo: 5/3) classAndValueEquals: f*f*f*f*f.
		self assert: ((f negated raisedTo: 5) raisedTo: 3/5) classAndValueEquals: (f*f*f) negated.
		self assert: ((f negated raisedTo: 3) raisedTo: 5/3) classAndValueEquals: (f*f*f*f*f) negated.

		"No exact result => Float result"
		self assert: ((f raisedTo: 3) +1 raisedTo: 5/3) isFloat.
		self assert: ((f negated raisedTo: 3) -1 raisedTo: 5/3) isFloat ].
]

{ #category : #'tests - mathematical functions' }
FractionTest >> testExactSqrt [
	"
	FractionTest new testExactSqrt
	"
	| f |
	self assert: (4/9) sqrt classAndValueEquals: 2/3.
	#( 1 5 29 135 1234 567890 123123123 456456456456 98765432109876543210987654321 987123987123987123987123987123987123987123987123) pairsDo: [ :i :j |
		f := i / j.
		self assert: f squared sqrt classAndValueEquals: f.
		f := j / i.
		self assert: f squared sqrt classAndValueEquals: f ]
]

{ #category : #'tests - conversions' }
FractionTest >> testFloor [
	self assert: (3 / 2) floor = 1.
	self assert: (-3 / 2) floor = -2.
]

{ #category : #'tests - mathematical functions' }
FractionTest >> testFloorLog [
	self assert: (1/100 floorLog: 10) = -2.
	self assert: (((2 raisedTo: Float emax + 11)/3) floorLog: 10)
		= ((Float emax + 11)*2 log - 3 log) floor description: 'Fraction>>log should not overflow'.
	self assert: ((3/(2 raisedTo: Float precision - Float emin)) floorLog: 10)
		= ((Float emin - Float precision)*2 log + 3 log) floor description: 'Fraction>>log should not underflow'
]

{ #category : #'tests - printing' }
FractionTest >> testFractionPrinting [

	self assert: (353/359) printString = '(353/359)'.
	self assert: ((2/3) printStringBase: 2) = '(10/11)'.
	self assert: ((2/3) storeStringBase: 2) = '(2r10/2r11)'.
	self assert: ((5/7) printStringBase: 3) = '(12/21)'.
	self assert: ((5/7) storeStringBase: 3) = '(3r12/3r21)'.
	self assert: ((11/13) printStringBase: 4) = '(23/31)'.
	self assert: ((11/13) storeStringBase: 4) = '(4r23/4r31)'.
	self assert: ((17/19) printStringBase: 5) = '(32/34)'.
	self assert: ((17/19) storeStringBase: 5) = '(5r32/5r34)'.
	self assert: ((23/29) printStringBase: 6) = '(35/45)'.
	self assert: ((23/29) storeStringBase: 6) = '(6r35/6r45)'.
	self assert: ((31/37) printStringBase: 7) = '(43/52)'.
	self assert: ((31/37) storeStringBase: 7) = '(7r43/7r52)'.
	self assert: ((41/43) printStringBase: 8) = '(51/53)'.
	self assert: ((41/43) storeStringBase: 8) = '(8r51/8r53)'.
	self assert: ((47/53) printStringBase: 9) = '(52/58)'.
	self assert: ((47/53) storeStringBase: 9) = '(9r52/9r58)'.
	self assert: ((59/61) printStringBase: 10) = '(59/61)'.
	self assert: ((59/61) storeStringBase: 10) = '(59/61)'.
	self assert: ((67/71) printStringBase: 11) = '(61/65)'.
	self assert: ((67/71) storeStringBase: 11) = '(11r61/11r65)'.
	self assert: ((73/79) printStringBase: 12) = '(61/67)'.
	self assert: ((73/79) storeStringBase: 12) = '(12r61/12r67)'.
	self assert: ((83/89) printStringBase: 13) = '(65/6B)'.
	self assert: ((83/89) storeStringBase: 13) = '(13r65/13r6B)'.
	self assert: ((97/101) printStringBase: 14) = '(6D/73)'.
	self assert: ((97/101) storeStringBase: 14) = '(14r6D/14r73)'.
	self assert: ((103/107) printStringBase: 15) = '(6D/72)'.
	self assert: ((103/107) storeStringBase: 15) = '(15r6D/15r72)'.
	self assert: ((109/113) printStringBase: 16) = '(6D/71)'.
	self assert: ((109/113) storeStringBase: 16) = '(16r6D/16r71)'.
	self assert: ((127/131) printStringBase: 17) = '(78/7C)'.
	self assert: ((127/131) storeStringBase: 17) = '(17r78/17r7C)'.
	self assert: ((137/139) printStringBase: 18) = '(7B/7D)'.
	self assert: ((137/139) storeStringBase: 18) = '(18r7B/18r7D)'.
	self assert: ((149/151) printStringBase: 19) = '(7G/7I)'.
	self assert: ((149/151) storeStringBase: 19) = '(19r7G/19r7I)'.
	self assert: ((157/163) printStringBase: 20) = '(7H/83)'.
	self assert: ((157/163) storeStringBase: 20) = '(20r7H/20r83)'.
	self assert: ((167/173) printStringBase: 21) = '(7K/85)'.
	self assert: ((167/173) storeStringBase: 21) = '(21r7K/21r85)'.
	self assert: ((179/181) printStringBase: 22) = '(83/85)'.
	self assert: ((179/181) storeStringBase: 22) = '(22r83/22r85)'.
	self assert: ((191/193) printStringBase: 23) = '(87/89)'.
	self assert: ((191/193) storeStringBase: 23) = '(23r87/23r89)'.
	self assert: ((197/199) printStringBase: 24) = '(85/87)'.
	self assert: ((197/199) storeStringBase: 24) = '(24r85/24r87)'.
	self assert: ((211/223) printStringBase: 25) = '(8B/8N)'.
	self assert: ((211/223) storeStringBase: 25) = '(25r8B/25r8N)'.
	self assert: ((227/229) printStringBase: 26) = '(8J/8L)'.
	self assert: ((227/229) storeStringBase: 26) = '(26r8J/26r8L)'.
	self assert: ((233/239) printStringBase: 27) = '(8H/8N)'.
	self assert: ((233/239) storeStringBase: 27) = '(27r8H/27r8N)'.
	self assert: ((241/251) printStringBase: 28) = '(8H/8R)'.
	self assert: ((241/251) storeStringBase: 28) = '(28r8H/28r8R)'.
	self assert: ((257/263) printStringBase: 29) = '(8P/92)'.
	self assert: ((257/263) storeStringBase: 29) = '(29r8P/29r92)'.
	self assert: ((269/271) printStringBase: 30) = '(8T/91)'.
	self assert: ((269/271) storeStringBase: 30) = '(30r8T/30r91)'.
	self assert: ((277/281) printStringBase: 31) = '(8T/92)'.
	self assert: ((277/281) storeStringBase: 31) = '(31r8T/31r92)'.
	self assert: ((283/293) printStringBase: 32) = '(8R/95)'.
	self assert: ((283/293) storeStringBase: 32) = '(32r8R/32r95)'.
	self assert: ((307/311) printStringBase: 33) = '(9A/9E)'.
	self assert: ((307/311) storeStringBase: 33) = '(33r9A/33r9E)'.
	self assert: ((313/317) printStringBase: 34) = '(97/9B)'.
	self assert: ((313/317) storeStringBase: 34) = '(34r97/34r9B)'.
	self assert: ((331/337) printStringBase: 35) = '(9G/9M)'.
	self assert: ((331/337) storeStringBase: 35) = '(35r9G/35r9M)'.
	self assert: ((347/349) printStringBase: 36) = '(9N/9P)'.
	self assert: ((347/349) storeStringBase: 36) = '(36r9N/36r9P)'.

	self assert: ((-2/3) printStringBase: 2) = '(-10/11)'.
	self assert: ((-2/3) storeStringBase: 2) = '(-2r10/2r11)'.
	self assert: ((5 / -7) printStringBase: 3) = '(-12/21)'.
	self assert: ((5 / -7) storeStringBase: 3) = '(-3r12/3r21)'.

]

{ #category : #'tests - mathematical functions' }
FractionTest >> testInexactRaisedTo [
	"
	FractionTest new testInexactRaisedTo
	"
	self assert: (((1 << 1024 + 1) / (1 << 1024 + 3)) raisedTo: 1/3) = 1.0.
	self assert: (((1 << 1024 + 1) / (1 << 1024 + 3)) negated raisedTo: 1/3) = -1.0
]

{ #category : #'tests - mathematical functions' }
FractionTest >> testInexactSqrt [
	"
	FractionTest new testInexactSqrt
	"
	self assert: ((1 << 1024 + 1) / (1 << 1024 + 3)) sqrt = 1.0
]

{ #category : #'tests - conversions' }
FractionTest >> testIsPowerOfTwo [
	"Negative fraction"
	self deny: (1 / (2 raisedTo: 80) negated) isPowerOfTwo.
	self deny: (1 negated / (2 raisedTo: 80)) isPowerOfTwo.
	self deny: ((2 raisedTo: 80) negated / 3) isPowerOfTwo.
	self deny: ((2 raisedTo: 80) / 4 negated) isPowerOfTwo.
	"Positive fraction"
	self assert: (1 / 2) isPowerOfTwo.
	self assert: (1 / (2 raisedTo: 80)) isPowerOfTwo.
	self assert: (1 negated / (2 raisedTo: 80) negated) isPowerOfTwo.
	self deny: ((2 raisedTo: 80) / 3) isPowerOfTwo.
	self assert: ((2 raisedTo: 80) / 4) isPowerOfTwo
]

{ #category : #'tests - reading' }
FractionTest >> testIvalidReadFrom [
	self should: [Fraction readFromString: '+3'] raise: Error description: 'numerator cannot specify a plus sign'.
	self should: [Fraction readFromString: '-2/+3'] raise: Error description: 'denominator cannot specify a plus sign'.
	self should: [Fraction readFromString: '(3/2)'] raise: Error description: 'parenthesis are not allowed'.
	self should: [Fraction readFromString: ' 3/25'] raise: Error description: 'leading spaces are not allowed before numerator'.
	self should: [Fraction readFromString: '22/ 3'] raise: Error description: 'leading spaces are not allowed before denominator'.
	
	"These behaviours are questionnable, but that's currently how it works:"
	self assert: (Fraction readFromString: '12345with some trailing characters') = 12345 description: 'non numeric trailing characters interrupt decoding'.
	self assert: (Fraction readFromString: '1 / 2') = 1 description: 'A space behind numerator interrupt decoding'.
	self assert: (Fraction readFromString: '22.0/3') = 22 description: 'decimal point interrupt decoding'.
	self assert: (Fraction readFromString: '23s0/3') = 23 description: 'scale specification interrupt decoding'.
]

{ #category : #'tests - mathematical functions' }
FractionTest >> testLn [
	self assert: ((1/100) ln closeTo: -2 * 10 ln).
	self assert: (((2 raisedTo: Float emax + 11)/3) ln closeTo: (Float emax + 11)*2 ln - 3 ln) description: 'Fraction>>ln should not overflow'.
	self assert: ((3/(2 raisedTo: Float precision - Float emin)) ln closeTo: (Float emin - Float precision)*2 ln + 3 ln) description: 'Fraction>>ln should not underflow'
]

{ #category : #'tests - mathematical functions' }
FractionTest >> testLog [
	self assert: ((1/100) log closeTo: -2).
	self assert: (((2 raisedTo: Float emax + 11)/3) log closeTo: (Float emax + 11)*2 log - 3 log) description: 'Fraction>>log should not overflow'.
	self assert: ((3/(2 raisedTo: Float precision - Float emin)) log closeTo: (Float emin - Float precision)*2 log + 3 log) description: 'Fraction>>log should not underflow'
]

{ #category : #'tests - mathematical functions' }
FractionTest >> testNthRoot [
	self assert: ((-2 raisedTo: 35) / (3 raisedTo: 20) raisedTo: 1/5) equals: (-2 raisedTo: 7) / (3 raisedTo: 4).
	self assert: (1 / (1 << 2000) raisedTo: 1/100) equals: 1 / (1 << 20)
]

{ #category : #'tests - mathematical functions' }
FractionTest >> testRaisedToErrorConditions [
	"
	FractionTest new testRaisedToErrorConditions
	"
	self should: [ (-1/16) raisedTo: 1/4 ] raise: ArithmeticError.
	self should: [ ((1 << 1024 + 1) / (1 << 1024 + 3)) negated raisedTo: 1/4 ] raise: ArithmeticError
]

{ #category : #'tests - reading' }
FractionTest >> testReadFrom [
	self assert: (Fraction readFromString: '3') = 3 description: 'denominator is optional'.
	self assert: (Fraction readFromString: '2/3') = (2/3) description: 'fraction and denominator must follow numerator'.
	self assert: (Fraction readFromString: '-2/3') = (-2/3) description: 'numerator can specify a minus sign'.
	.self assert: (Fraction readFromString: '2/-5') = (-2/5) description: 'denominator can specify a minus sign'.
	self assert: (Fraction readFromString: '-3/-7') = (3/7) description: 'numerator and denominator can both specify a minus sign'.
	self assert: (Fraction readFromString: '2e3/3') = (2000/3) description: 'numerator can specify an exponent'.
	self assert: (Fraction readFromString: '3/1e4') = (3/10000) description: 'denominator can specify an exponent'.
	self assert: (Fraction readFromString: '16rA0/3') = (160/3) description: 'numerator can specify a radix'.
	self assert: (Fraction readFromString: '1/3r22') = (1/8) description: 'denominator can specify a radix'.
]

{ #category : #'tests - sinuses' }
FractionTest >> testReciprocal [

	self 
		assert: (1/2) reciprocal = 2;
		assert: (3/4) reciprocal = (4/3);
		assert: (-1/3) reciprocal = -3;
		assert: (-3/5) reciprocal = (-5/3)
]

{ #category : #'tests - conversions' }
FractionTest >> testRounded [
	self assert: (4 / 5) rounded = 1.
	self assert: (6 / 5) rounded = 1.
	self assert: (-4 / 5) rounded = -1.
	self assert: (-6 / 5) rounded = -1.
	
	"In case of tie, round to upper magnitude"
	self assert: (3 / 2) rounded = 2.
	self assert: (-3 / 2) rounded = -2.
]

{ #category : #'tests - rounding' }
FractionTest >> testRounding [
        "
        self debug: #testRounding
        "

        self assert: ((6/90) round: 2) equals: 7/100
]

{ #category : #'tests - mathematical functions' }
FractionTest >> testSqrtErrorConditions [
	"
	FractionTest new testSqrtErrorConditions
	"
	self should: [ (-1/4) sqrt ] raise: DomainError.
	self should: [ ((1 << 1024 + 1) / (1 << 1024 + 3)) negated sqrt ] raise: DomainError
]

{ #category : #'tests - conversions' }
FractionTest >> testTruncated [
	self assert: (3 / 2) truncated = 1.
	self assert: (-3 / 2) truncated = -1.
]
