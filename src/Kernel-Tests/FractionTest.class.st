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
	self assert: a class equals: b class.
	self assert: a equals: b
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
	self assert: (3 / 2) ceiling equals: 2.
	self assert: (-3 / 2) ceiling equals: -1
]

{ #category : #'tests - conversions' }
FractionTest >> testFloor [
	self assert: (3 / 2) floor equals: 1.
	self assert: (-3 / 2) floor equals: -2
]

{ #category : #'tests - printing' }
FractionTest >> testFractionPrinting [
	self assert: (353 / 359) printString equals: '(353/359)'.
	self assert: (2 / 3 printStringBase: 2) equals: '(10/11)'.
	self assert: (2 / 3 storeStringBase: 2) equals: '(2r10/2r11)'.
	self assert: (5 / 7 printStringBase: 3) equals: '(12/21)'.
	self assert: (5 / 7 storeStringBase: 3) equals: '(3r12/3r21)'.
	self assert: (11 / 13 printStringBase: 4) equals: '(23/31)'.
	self assert: (11 / 13 storeStringBase: 4) equals: '(4r23/4r31)'.
	self assert: (17 / 19 printStringBase: 5) equals: '(32/34)'.
	self assert: (17 / 19 storeStringBase: 5) equals: '(5r32/5r34)'.
	self assert: (23 / 29 printStringBase: 6) equals: '(35/45)'.
	self assert: (23 / 29 storeStringBase: 6) equals: '(6r35/6r45)'.
	self assert: (31 / 37 printStringBase: 7) equals: '(43/52)'.
	self assert: (31 / 37 storeStringBase: 7) equals: '(7r43/7r52)'.
	self assert: (41 / 43 printStringBase: 8) equals: '(51/53)'.
	self assert: (41 / 43 storeStringBase: 8) equals: '(8r51/8r53)'.
	self assert: (47 / 53 printStringBase: 9) equals: '(52/58)'.
	self assert: (47 / 53 storeStringBase: 9) equals: '(9r52/9r58)'.
	self assert: (59 / 61 printStringBase: 10) equals: '(59/61)'.
	self assert: (59 / 61 storeStringBase: 10) equals: '(59/61)'.
	self assert: (67 / 71 printStringBase: 11) equals: '(61/65)'.
	self assert: (67 / 71 storeStringBase: 11) equals: '(11r61/11r65)'.
	self assert: (73 / 79 printStringBase: 12) equals: '(61/67)'.
	self assert: (73 / 79 storeStringBase: 12) equals: '(12r61/12r67)'.
	self assert: (83 / 89 printStringBase: 13) equals: '(65/6B)'.
	self assert: (83 / 89 storeStringBase: 13) equals: '(13r65/13r6B)'.
	self assert: (97 / 101 printStringBase: 14) equals: '(6D/73)'.
	self assert: (97 / 101 storeStringBase: 14) equals: '(14r6D/14r73)'.
	self assert: (103 / 107 printStringBase: 15) equals: '(6D/72)'.
	self assert: (103 / 107 storeStringBase: 15) equals: '(15r6D/15r72)'.
	self assert: (109 / 113 printStringBase: 16) equals: '(6D/71)'.
	self assert: (109 / 113 storeStringBase: 16) equals: '(16r6D/16r71)'.
	self assert: (127 / 131 printStringBase: 17) equals: '(78/7C)'.
	self assert: (127 / 131 storeStringBase: 17) equals: '(17r78/17r7C)'.
	self assert: (137 / 139 printStringBase: 18) equals: '(7B/7D)'.
	self assert: (137 / 139 storeStringBase: 18) equals: '(18r7B/18r7D)'.
	self assert: (149 / 151 printStringBase: 19) equals: '(7G/7I)'.
	self assert: (149 / 151 storeStringBase: 19) equals: '(19r7G/19r7I)'.
	self assert: (157 / 163 printStringBase: 20) equals: '(7H/83)'.
	self assert: (157 / 163 storeStringBase: 20) equals: '(20r7H/20r83)'.
	self assert: (167 / 173 printStringBase: 21) equals: '(7K/85)'.
	self assert: (167 / 173 storeStringBase: 21) equals: '(21r7K/21r85)'.
	self assert: (179 / 181 printStringBase: 22) equals: '(83/85)'.
	self assert: (179 / 181 storeStringBase: 22) equals: '(22r83/22r85)'.
	self assert: (191 / 193 printStringBase: 23) equals: '(87/89)'.
	self assert: (191 / 193 storeStringBase: 23) equals: '(23r87/23r89)'.
	self assert: (197 / 199 printStringBase: 24) equals: '(85/87)'.
	self assert: (197 / 199 storeStringBase: 24) equals: '(24r85/24r87)'.
	self assert: (211 / 223 printStringBase: 25) equals: '(8B/8N)'.
	self assert: (211 / 223 storeStringBase: 25) equals: '(25r8B/25r8N)'.
	self assert: (227 / 229 printStringBase: 26) equals: '(8J/8L)'.
	self assert: (227 / 229 storeStringBase: 26) equals: '(26r8J/26r8L)'.
	self assert: (233 / 239 printStringBase: 27) equals: '(8H/8N)'.
	self assert: (233 / 239 storeStringBase: 27) equals: '(27r8H/27r8N)'.
	self assert: (241 / 251 printStringBase: 28) equals: '(8H/8R)'.
	self assert: (241 / 251 storeStringBase: 28) equals: '(28r8H/28r8R)'.
	self assert: (257 / 263 printStringBase: 29) equals: '(8P/92)'.
	self assert: (257 / 263 storeStringBase: 29) equals: '(29r8P/29r92)'.
	self assert: (269 / 271 printStringBase: 30) equals: '(8T/91)'.
	self assert: (269 / 271 storeStringBase: 30) equals: '(30r8T/30r91)'.
	self assert: (277 / 281 printStringBase: 31) equals: '(8T/92)'.
	self assert: (277 / 281 storeStringBase: 31) equals: '(31r8T/31r92)'.
	self assert: (283 / 293 printStringBase: 32) equals: '(8R/95)'.
	self assert: (283 / 293 storeStringBase: 32) equals: '(32r8R/32r95)'.
	self assert: (307 / 311 printStringBase: 33) equals: '(9A/9E)'.
	self assert: (307 / 311 storeStringBase: 33) equals: '(33r9A/33r9E)'.
	self assert: (313 / 317 printStringBase: 34) equals: '(97/9B)'.
	self assert: (313 / 317 storeStringBase: 34) equals: '(34r97/34r9B)'.
	self assert: (331 / 337 printStringBase: 35) equals: '(9G/9M)'.
	self assert: (331 / 337 storeStringBase: 35) equals: '(35r9G/35r9M)'.
	self assert: (347 / 349 printStringBase: 36) equals: '(9N/9P)'.
	self assert: (347 / 349 storeStringBase: 36) equals: '(36r9N/36r9P)'.

	self assert: (-2 / 3 printStringBase: 2) equals: '(-10/11)'.
	self assert: (-2 / 3 storeStringBase: 2) equals: '(-2r10/2r11)'.
	self assert: (5 / -7 printStringBase: 3) equals: '(-12/21)'.
	self assert: (5 / -7 storeStringBase: 3) equals: '(-3r12/3r21)'
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
	self assert: (Fraction readFromString: '2/-5') = (-2/5) description: 'denominator can specify a minus sign'.
	self assert: (Fraction readFromString: '-3/-7') = (3/7) description: 'numerator and denominator can both specify a minus sign'.
	self assert: (Fraction readFromString: '2e3/3') = (2000/3) description: 'numerator can specify an exponent'.
	self assert: (Fraction readFromString: '3/1e4') = (3/10000) description: 'denominator can specify an exponent'.
	self assert: (Fraction readFromString: '16rA0/3') = (160/3) description: 'numerator can specify a radix'.
	self assert: (Fraction readFromString: '1/3r22') = (1/8) description: 'denominator can specify a radix'.
]

{ #category : #'tests - sinuses' }
FractionTest >> testReciprocal [
	self
		assert: (1 / 2) reciprocal equals: 2;
		assert: (3 / 4) reciprocal equals: 4 / 3;
		assert: (-1 / 3) reciprocal equals: -3;
		assert: (-3 / 5) reciprocal equals: -5 / 3
]

{ #category : #'tests - conversions' }
FractionTest >> testRounded [
	self assert: (4 / 5) rounded equals: 1.
	self assert: (6 / 5) rounded equals: 1.
	self assert: (-4 / 5) rounded equals: -1.
	self assert: (-6 / 5) rounded equals: -1.

	"In case of tie, round to upper magnitude"
	self assert: (3 / 2) rounded equals: 2.
	self assert: (-3 / 2) rounded equals: -2
]

{ #category : #'tests - rounding' }
FractionTest >> testRounding [
	
	self assert: ((6/90) round: 2) equals: 7/100
]

{ #category : #'tests - conversions' }
FractionTest >> testTruncated [
	self assert: (3 / 2) truncated equals: 1.
	self assert: (-3 / 2) truncated equals: -1
]
