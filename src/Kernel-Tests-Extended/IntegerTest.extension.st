Extension { #name : #IntegerTest }

{ #category : #'*Kernel-Tests-Extended' }
IntegerTest >> testBenchFib [
	self assert: 0 benchFib equals: 1.
	self assert: 1 benchFib equals: 1.
	self assert: 2 benchFib equals: 3
]

{ #category : #'*Kernel-Tests-Extended' }
IntegerTest >> testBigReceiverInexactNthRoot [
	"
	IntegerTest new testBigReceiverInexactNthRoot
	"

	"Inexact 3rd root (not a whole cube number), so a Float must be answered.
	However, receiver is too big for Float arithmethic."

	| bigNum result |
	bigNum := (100 factorial raisedTo: 3) + 1.	"Add 1 so it is not a whole cube"
	self assert: bigNum asFloat isInfinite.	"Otherwise, we chose a bad sample"
	result := bigNum nthRoot: 3.
	self assert: result isFloat.
	self deny: result isInfinite.
	self assert: result equals: 100 factorial asFloat.	"No other float is closer. See following line"
	self assert: 100 factorial asFloat equals: (100 factorial + 1) asFloat
]

{ #category : #'*Kernel-Tests-Extended' }
IntegerTest >> testBigReceiverInexactSqrt [
	"
	IntegerTest new testBigReceiverInexactSqrt
	"

	"Inexact 3rd root (not a whole cube number), so a Float must be answered.
	However, receiver is too big for Float arithmethic."

	| bigNum result |
	bigNum := 100 factorial squared + 1.	"Add 1 so it is not a whole square"
	self assert: bigNum asFloat isInfinite.	"Otherwise, we chose a bad sample"
	result := bigNum sqrt.
	self assert: result isFloat.
	self deny: result isInfinite.
	self assert: result equals: 100 factorial asFloat.	"No other float is closer. See following lines"
	self assert: (result successor asFraction squared - bigNum) abs >= (result asFraction squared - bigNum) abs.
	self assert: (result predecessor asFraction squared - bigNum) abs >= (result asFraction squared - bigNum) abs
]

{ #category : #'*Kernel-Tests-Extended' }
IntegerTest >> testBitOr [

	self assert: (2r0101 | 2r1010) equals: 2r1111.
	self assert: (2r0101 bitOr: 2r1010) equals: 2r1111.
]

{ #category : #'*Kernel-Tests-Extended' }
IntegerTest >> testDegreeCos [

	45 degreeCos.	"Following tests use approximate equality, because cosine are generally evaluated using inexact Floating point arithmetic"
	self assert: (45 degreeCos squared - (1 / 2)) abs <= Float epsilon.
	self assert: (60 degreeCos - (1 / 2)) abs <= Float epsilon.
	self assert: (120 degreeCos + (1 / 2)) abs <= Float epsilon.
	-360 to: 360 do: [ :i | self assert: (i degreeCos closeTo: i degreesToRadians cos) ].	"Following tests use strict equality which is a requested property of degreeCos"
	-10 to: 10 do: [ :k | 
		self assert: (k * 360 + 90) degreeCos equals: 0.
		self assert: (k * 360 - 90) degreeCos equals: 0.
		self assert: (k * 360 + 180) degreeCos + 1 equals: 0.
		self assert: (k * 360) degreeCos - 1 equals: 0 ]
]

{ #category : #'*Kernel-Tests-Extended' }
IntegerTest >> testDegreeSin [

	45 degreeSin.	"Following tests use approximate equality, because sine are generally evaluated using inexact Floating point arithmetic"
	self assert: (45 degreeSin squared - (1 / 2)) abs <= Float epsilon.
	self assert: (30 degreeSin - (1 / 2)) abs <= Float epsilon.
	self assert: (-30 degreeSin + (1 / 2)) abs <= Float epsilon.
	-360 to: 360 do: [ :i | self assert: (i degreeSin closeTo: i degreesToRadians sin) ].	"Following tests use strict equality which is a requested property of degreeSin"
	-10 to: 10 do: [ :k | 
		self assert: (k * 360 + 90) degreeSin - 1 equals: 0.
		self assert: (k * 360 - 90) degreeSin + 1 equals: 0.
		self assert: (k * 360 + 180) degreeSin equals: 0.
		self assert: (k * 360) degreeSin equals: 0 ]
]

{ #category : #'*Kernel-Tests-Extended' }
IntegerTest >> testDigitsAccess [

	self assert: (42 digitAt: 2 base: 10) equals: 4.
	self assert: (42 digitAt: 1 base: 10) equals: 2.
	self assert: (1 digitAt: 2 base: 10) equals: 0.
	self assert: (16rFF digitAt: 1 base: 16) equals: 15.

	self assert: (42 decimalDigitAt: 2) equals: 4.
	self assert: (42 decimalDigitAt: 1) equals: 2.
	
	self assert: (42 decimalDigitLength) equals: 2.
	self assert: (0 decimalDigitLength) equals: 1.
	self assert: (100000000000 decimalDigitLength) equals: 12. "large integer"

	"last digit of the integer base 256"
	self assert: (255 lastDigit) equals: 255.
	self assert: (256 lastDigit) equals: 1.
	self assert: ((256*256-1) lastDigit) equals: 255.
	self assert: ((256*256) lastDigit) equals: 1.

	
]

{ #category : #'*Kernel-Tests-Extended' }
IntegerTest >> testExactRaisedTo [
	"
	IntegerTest new testExactRaisedTo
	"
	self assert: (4 raisedTo: 1/2) classAndValueEquals: 2.
	self assert: (9 raisedTo: 1/2) classAndValueEquals: 3.
	self assert: (9 raisedTo: -1/2) classAndValueEquals: 1/3.
	self assert: (-1 raisedTo: 1/3) classAndValueEquals: -1.
	#( 1 5 29 135 1234 567890 123123123 456456456456 98765432109876543210987654321 987123987123987123987123987123987123987123987123) do: [ :i |
		self assert: (i squared raisedTo: 1/2) classAndValueEquals: i.
		self assert: (i negated squared raisedTo: 1/2) classAndValueEquals: i ].

	self assert: (8 raisedTo: 1/3) classAndValueEquals: 2.
	self assert: (27 raisedTo: 1/3) classAndValueEquals: 3.
	#( 1 5 29 135 1234 567890 123123123 456456456456 98765432109876543210987654321 987123987123987123987123987123987123987123987123) do: [ :i |
		self assert: ((i raisedTo: 3) raisedTo: 1/3) classAndValueEquals: i.
		self assert: ((i negated raisedTo: 3) raisedTo: 1/3) classAndValueEquals: i negated ].

	self assert: (4 raisedTo: 3/2) classAndValueEquals: 8.
	self assert: (8 raisedTo: 2/3) classAndValueEquals: 4.
	self assert: (8 raisedTo: -2/3) classAndValueEquals: 1/4.
	#( 1 5 29 135 1234 567890 123123123 456456456456 98765432109876543210987654321 987123987123987123987123987123987123987123987123) do: [ :i |
		self assert: ((i raisedTo: 3) raisedTo: 2/3) classAndValueEquals: i*i.
		self assert: ((i raisedTo: 2) raisedTo: 3/2) classAndValueEquals: i*i*i.
		self assert: ((i negated raisedTo: 3) raisedTo: 2/3) classAndValueEquals: i*i.
		self assert: ((i negated raisedTo: 2) raisedTo: 3/2) classAndValueEquals: i*i*i ].

	self assert: (32 raisedTo: 3/5) classAndValueEquals: 8.
	self assert: (8 raisedTo: 5/3) classAndValueEquals: 32.
	#( 1 5 29 135 1234 567890 123123123 456456456456 98765432109876543210987654321 987123987123987123987123987123987123987123987123) do: [ :i |
		self assert: ((i raisedTo: 5) raisedTo: 3/5) classAndValueEquals: i*i*i.
		self assert: ((i raisedTo: 3) raisedTo: 5/3) classAndValueEquals: i*i*i*i*i.
		self assert: ((i negated raisedTo: 5) raisedTo: 3/5) classAndValueEquals: (i*i*i) negated.
		self assert: ((i negated raisedTo: 3) raisedTo: 5/3) classAndValueEquals: (i*i*i*i*i) negated.

		self assert: ((i raisedTo: -5) raisedTo: 3/5) classAndValueEquals: 1/(i*i*i).
		self assert: ((i raisedTo: -3) raisedTo: 5/3) classAndValueEquals: 1/(i*i*i*i*i).
		self assert: ((i negated raisedTo: -5) raisedTo: 3/5) classAndValueEquals: -1/(i*i*i).
		self assert: ((i negated raisedTo: -3) raisedTo: 5/3) classAndValueEquals: -1/(i*i*i*i*i).

		self assert: ((i raisedTo: 5) raisedTo: -3/5) classAndValueEquals: 1/(i*i*i).
		self assert: ((i raisedTo: 3) raisedTo: -5/3) classAndValueEquals: 1/(i*i*i*i*i).
		self assert: ((i negated raisedTo: 5) raisedTo: -3/5) classAndValueEquals: -1/(i*i*i).
		self assert: ((i negated raisedTo: 3) raisedTo: -5/3) classAndValueEquals: -1/(i*i*i*i*i).

		"No exact result => Float result"
		self assert: ((i raisedTo: 3) +1 raisedTo: 5/3) isFloat.
		self assert: ((i negated raisedTo: 3) -1 raisedTo: 5/3) isFloat ].
]

{ #category : #'*Kernel-Tests-Extended' }
IntegerTest >> testExactSqrt [
	"
	IntegerTest new testExactSqrt
	"
	self assert: 4 sqrt classAndValueEquals: 2.
	self assert: 9 sqrt classAndValueEquals: 3.
	self assert: Float maxExactInteger squared sqrt classAndValueEquals: Float maxExactInteger.
	self assert: (Float maxExactInteger+1) squared sqrt classAndValueEquals: Float maxExactInteger+1.
	#( 1 5 29 135 1234 567890 123123123 456456456456 98765432109876543210987654321 987123987123987123987123987123987123987123987123) do: [ :i |
		self assert: i squared sqrt classAndValueEquals: i ]
]

{ #category : #'*Kernel-Tests-Extended' }
IntegerTest >> testFloorLog [
	self assert: (100 floorLog: 10) equals: 2.
	self assert: ((2 raisedTo: Float emax + 3) floorLog: 10) = (2 log * (Float emax + 3)) floor description: 'Integer>>floorLog: should not overflow'
]

{ #category : #'*Kernel-Tests-Extended' }
IntegerTest >> testIsPrime [

	"The following tests should return 'true'"
	self assert: 17 isPrime.
	self assert: 78901 isPrime.
	self assert: 104729 isPrime.
	self assert: 15485863 isPrime.
	self assert: 2038074743 isPrime.
	self assert: 29996224275833 isPrime.
	
	"The following tests should return 'false' (first 5 are Carmichael integers)"
	self deny: 561 isPrime.
	self deny: 2821 isPrime.
	self deny: 6601 isPrime.
	self deny: 10585 isPrime.
	self deny: 15841 isPrime.
	self deny: 256 isPrime.
	self deny: 29996224275831 isPrime.
]

{ #category : #'*Kernel-Tests-Extended' }
IntegerTest >> testIsPrime2 [

	"Not primes:"
	#(-100 -5 -3 -2 -1 0 1) do: [ :each |
		self deny: each isPrime ].

	"The following tests should return 'true'"
	#(17 78901 104729 15485863 2038074743) do: [ :each |
		self assert: each isPrime ].
	
	"The following tests should return 'false' (first 5 are Carmichael integers)"
	#(561 2821 6601 10585 15841 256 29996224275831) do: [ :each |
		self deny: each isPrime ].
]

{ #category : #'*Kernel-Tests-Extended' }
IntegerTest >> testIsProbablyPrime [

	"Not primes:"
	#(-100 -5 -3 -2 -1 0 1) do: [ :each |
		self deny: each isProbablyPrime ].

	"The following tests should return 'true'"
	#(17 78901 104729 15485863 2038074743 29996224275833) do: [ :each |
		self assert: each isProbablyPrime ].
	
	"The following tests should return 'false' (first 5 are Carmichael integers)"
	#(561 2821 6601 10585 15841 256 29996224275831) do: [ :each |
		self deny: each isProbablyPrime ].
]

{ #category : #'*Kernel-Tests-Extended' }
IntegerTest >> testLargePrimesUpTo [
	| nn |
	nn := (2 raisedTo: 17) - 1.
	self deny: (Integer primesUpTo: nn) last equals: nn.
	self assert: (Integer primesUpTo: nn + 1) last equals: nn
]

{ #category : #'*Kernel-Tests-Extended' }
IntegerTest >> testLn [
	self assert: (100 ln closeTo: 10 ln*2).
	self assert: ((2 raisedTo: Float emax + 3) ln closeTo: 2 ln*(Float emax + 3)) description: 'Integer>>ln should not overflow'
]

{ #category : #'*Kernel-Tests-Extended' }
IntegerTest >> testLog [
	self assert: (100 log closeTo: 2).
	self assert: ((2 raisedTo: Float emax + 3) log closeTo: 2 log*(Float emax + 3)) description: 'Integer>>log should not overflow'
]

{ #category : #'*Kernel-Tests-Extended' }
IntegerTest >> testNthRoot [
	1 << 2000 nthRoot: 100.
	self assert: (1 << 2000 nthRoot: 100) equals: 1 << 20
]

{ #category : #'*Kernel-Tests-Extended' }
IntegerTest >> testNthRootErrorConditions [
	"
	IntegerTest new testExactRaisedToErrorConditions
	"

	self should: [ -2 nthRoot: 1/4 ] raise: ArithmeticError.
	self should: [ -2 nthRoot: 1.24 ] raise: ArithmeticError.
]

{ #category : #'*Kernel-Tests-Extended' }
IntegerTest >> testNthRootExactness [
	| inexactRoots largeExactPowersOf6 |
	largeExactPowersOf6 := (2 to: 100) collect: [:k | k raisedTo: 66].
	inexactRoots := largeExactPowersOf6 reject: [:e | (e nthRoot: 6) isInteger].
	self assert: inexactRoots isEmpty description: 'Failed to find the exact 6th root of these numbers'
]

{ #category : #'*Kernel-Tests-Extended' }
IntegerTest >> testNthRootTruncated [
	<timeout:  5 "seconds">
	| tooBigToBeAFloat large |
	tooBigToBeAFloat := 1 << 2000.
	self assert: (tooBigToBeAFloat nthRootTruncated: 100) equals: 1 << 20.
	self assert: (tooBigToBeAFloat + 1 nthRootTruncated: 100) equals: 1 << 20.
	self assert: (tooBigToBeAFloat - 1 nthRootTruncated: 100) equals: 1 << 20 - 1.
	
	large := -3 raisedTo: 255.
	self assert: (large nthRootTruncated: 17) equals: (-3 raisedTo: 15).
	self assert: (large + 11 nthRootTruncated: 17) equals: (-3 raisedTo: 15) + 1.
	self assert: (large - 11 nthRootTruncated: 17) equals: (-3 raisedTo: 15).
	
	2 to: 10 do: [:thePower |
		1 to: 10000 do: [:n |
			| theTruncatedRoot |
			theTruncatedRoot := n nthRootTruncated: thePower.
			self assert: (theTruncatedRoot raisedTo: thePower) <= n.
			self assert: (theTruncatedRoot + 1 raisedTo: thePower) > n]]
]

{ #category : #'*Kernel-Tests-Extended' }
IntegerTest >> testPrimesUpTo [
	| primes nn |
	primes := Integer primesUpTo: 100.
	self assert: primes equals: #(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97).

	"upTo: semantics means 'non-inclusive'"
	primes := Integer primesUpTo: 5.
	self assert: primes equals: #(2 3).

	"this test is green for nn>25000, see #testLargePrimesUpTo"
	nn := 5.
	self deny: (Integer primesUpTo: nn) last equals: nn.
	self assert: (Integer primesUpTo: nn + 1) last equals: nn
]

{ #category : #'*Kernel-Tests-Extended' }
IntegerTest >> testRomanPrinting [
	self assert: 0 printStringRoman equals: ''.	"No symbol for zero"
	self assert: 1 printStringRoman equals: 'I'.
	self assert: 2 printStringRoman equals: 'II'.
	self assert: 3 printStringRoman equals: 'III'.
	self assert: 4 printStringRoman equals: 'IV'.
	self assert: 5 printStringRoman equals: 'V'.
	self assert: 6 printStringRoman equals: 'VI'.
	self assert: 7 printStringRoman equals: 'VII'.
	self assert: 8 printStringRoman equals: 'VIII'.
	self assert: 9 printStringRoman equals: 'IX'.
	self assert: 10 printStringRoman equals: 'X'.
	self assert: 23 printStringRoman equals: 'XXIII'.
	self assert: 36 printStringRoman equals: 'XXXVI'.
	self assert: 49 printStringRoman equals: 'XLIX'.
	self assert: 62 printStringRoman equals: 'LXII'.
	self assert: 75 printStringRoman equals: 'LXXV'.
	self assert: 88 printStringRoman equals: 'LXXXVIII'.
	self assert: 99 printStringRoman equals: 'XCIX'.
	self assert: 100 printStringRoman equals: 'C'.
	self assert: 101 printStringRoman equals: 'CI'.
	self assert: 196 printStringRoman equals: 'CXCVI'.
	self assert: 197 printStringRoman equals: 'CXCVII'.
	self assert: 198 printStringRoman equals: 'CXCVIII'.
	self assert: 293 printStringRoman equals: 'CCXCIII'.
	self assert: 294 printStringRoman equals: 'CCXCIV'.
	self assert: 295 printStringRoman equals: 'CCXCV'.
	self assert: 390 printStringRoman equals: 'CCCXC'.
	self assert: 391 printStringRoman equals: 'CCCXCI'.
	self assert: 392 printStringRoman equals: 'CCCXCII'.
	self assert: 487 printStringRoman equals: 'CDLXXXVII'.
	self assert: 488 printStringRoman equals: 'CDLXXXVIII'.
	self assert: 489 printStringRoman equals: 'CDLXXXIX'.
	self assert: 584 printStringRoman equals: 'DLXXXIV'.
	self assert: 585 printStringRoman equals: 'DLXXXV'.
	self assert: 586 printStringRoman equals: 'DLXXXVI'.
	self assert: 681 printStringRoman equals: 'DCLXXXI'.
	self assert: 682 printStringRoman equals: 'DCLXXXII'.
	self assert: 683 printStringRoman equals: 'DCLXXXIII'.
	self assert: 778 printStringRoman equals: 'DCCLXXVIII'.
	self assert: 779 printStringRoman equals: 'DCCLXXIX'.
	self assert: 780 printStringRoman equals: 'DCCLXXX'.
	self assert: 875 printStringRoman equals: 'DCCCLXXV'.
	self assert: 876 printStringRoman equals: 'DCCCLXXVI'.
	self assert: 877 printStringRoman equals: 'DCCCLXXVII'.
	self assert: 972 printStringRoman equals: 'CMLXXII'.
	self assert: 973 printStringRoman equals: 'CMLXXIII'.
	self assert: 974 printStringRoman equals: 'CMLXXIV'.
	self assert: 1069 printStringRoman equals: 'MLXIX'.
	self assert: 1070 printStringRoman equals: 'MLXX'.
	self assert: 1071 printStringRoman equals: 'MLXXI'.
	self assert: 1166 printStringRoman equals: 'MCLXVI'.
	self assert: 1167 printStringRoman equals: 'MCLXVII'.
	self assert: 1168 printStringRoman equals: 'MCLXVIII'.
	self assert: 1263 printStringRoman equals: 'MCCLXIII'.
	self assert: 1264 printStringRoman equals: 'MCCLXIV'.
	self assert: 1265 printStringRoman equals: 'MCCLXV'.
	self assert: 1360 printStringRoman equals: 'MCCCLX'.
	self assert: 1361 printStringRoman equals: 'MCCCLXI'.
	self assert: 1362 printStringRoman equals: 'MCCCLXII'.
	self assert: 1457 printStringRoman equals: 'MCDLVII'.
	self assert: 1458 printStringRoman equals: 'MCDLVIII'.
	self assert: 1459 printStringRoman equals: 'MCDLIX'.
	self assert: 1554 printStringRoman equals: 'MDLIV'.
	self assert: 1555 printStringRoman equals: 'MDLV'.
	self assert: 1556 printStringRoman equals: 'MDLVI'.
	self assert: 1651 printStringRoman equals: 'MDCLI'.
	self assert: 1652 printStringRoman equals: 'MDCLII'.
	self assert: 1653 printStringRoman equals: 'MDCLIII'.
	self assert: 1748 printStringRoman equals: 'MDCCXLVIII'.
	self assert: 1749 printStringRoman equals: 'MDCCXLIX'.
	self assert: 1750 printStringRoman equals: 'MDCCL'.
	self assert: 1845 printStringRoman equals: 'MDCCCXLV'.
	self assert: 1846 printStringRoman equals: 'MDCCCXLVI'.
	self assert: 1847 printStringRoman equals: 'MDCCCXLVII'.
	self assert: 1942 printStringRoman equals: 'MCMXLII'.
	self assert: 1943 printStringRoman equals: 'MCMXLIII'.
	self assert: 1944 printStringRoman equals: 'MCMXLIV'.
	self assert: 2004 printStringRoman equals: 'MMIV'.

	self assert: -1 printStringRoman equals: '-I'.
	self assert: -2 printStringRoman equals: '-II'.
	self assert: -3 printStringRoman equals: '-III'.
	self assert: -4 printStringRoman equals: '-IV'.
	self assert: -5 printStringRoman equals: '-V'.
	self assert: -6 printStringRoman equals: '-VI'.
	self assert: -7 printStringRoman equals: '-VII'.
	self assert: -8 printStringRoman equals: '-VIII'.
	self assert: -9 printStringRoman equals: '-IX'.
	self assert: -10 printStringRoman equals: '-X'
]

{ #category : #'*Kernel-Tests-Extended' }
IntegerTest >> testSqrtErrorConditions [
	"
	IntegerTest new testSqrtErrorConditions
	"

	self should: [ -1 sqrt ] raise: ArithmeticError
]

{ #category : #'*Kernel-Tests-Extended' }
IntegerTest >> testSqrtFloor [
	#(-1234567890123 -10 -5 -1) do: [ :each | self should: [ each sqrtFloor ] raise: Error ].
	#(0 1 2 3 4 5 10 16 30 160479924 386234481 501619156 524723498 580855366 766098594 834165249 1020363860 1042083924 1049218924 1459774772895569 3050005981408238 4856589481837079 5650488387708463 7831037396100244)
		do: [ :each | self assert: each asFloat sqrt floor equals: each sqrtFloor ]
]
