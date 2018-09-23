Extension { #name : #IntegerTest }

{ #category : #'*Kernel-Tests-Extended' }
IntegerTest >> testNthRootErrorConditions [
	"
	IntegerTest new testExactRaisedToErrorConditions
	"

	self should: [ -2 nthRoot: 1/4 ] raise: ArithmeticError.
	self should: [ -2 nthRoot: 1.24 ] raise: ArithmeticError.
]

{ #category : #'*Kernel-Tests-Extended' }
IntegerTest >> testPrimesUpTo [

	| primes nn|
	primes := Integer primesUpTo: 100.
	self assert: primes = #(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97).
	
	"upTo: semantics means 'non-inclusive'"
	primes := Integer primesUpTo: 5.
	self assert: primes = #(2 3).
	
	"this test is green for nn>25000, see #testLargePrimesUpTo"
	nn := 5. 
	self deny: (Integer primesUpTo: nn) last = nn.
	self assert: (Integer primesUpTo: nn + 1) last  = nn.
]

{ #category : #'*Kernel-Tests-Extended' }
IntegerTest >> testRomanPrinting [
	self assert: 0 printStringRoman = ''. "No symbol for zero"
	self assert: 1 printStringRoman = 'I'.
	self assert: 2 printStringRoman = 'II'.
	self assert: 3 printStringRoman = 'III'.
	self assert: 4 printStringRoman = 'IV'.
	self assert: 5 printStringRoman = 'V'.
	self assert: 6 printStringRoman = 'VI'.
	self assert: 7 printStringRoman = 'VII'.
	self assert: 8 printStringRoman = 'VIII'.
	self assert: 9 printStringRoman = 'IX'.
	self assert: 10 printStringRoman = 'X'.
	self assert: 23 printStringRoman = 'XXIII'.
	self assert: 36 printStringRoman = 'XXXVI'.
	self assert: 49 printStringRoman = 'XLIX'.
	self assert: 62 printStringRoman = 'LXII'.
	self assert: 75 printStringRoman = 'LXXV'.
	self assert: 88 printStringRoman = 'LXXXVIII'.
	self assert: 99 printStringRoman = 'XCIX'.
	self assert: 100 printStringRoman = 'C'.
	self assert: 101 printStringRoman = 'CI'.
	self assert: 196 printStringRoman = 'CXCVI'.
	self assert: 197 printStringRoman = 'CXCVII'.
	self assert: 198 printStringRoman = 'CXCVIII'.
	self assert: 293 printStringRoman = 'CCXCIII'.
	self assert: 294 printStringRoman = 'CCXCIV'.
	self assert: 295 printStringRoman = 'CCXCV'.
	self assert: 390 printStringRoman = 'CCCXC'.
	self assert: 391 printStringRoman = 'CCCXCI'.
	self assert: 392 printStringRoman = 'CCCXCII'.
	self assert: 487 printStringRoman = 'CDLXXXVII'.
	self assert: 488 printStringRoman = 'CDLXXXVIII'.
	self assert: 489 printStringRoman = 'CDLXXXIX'.
	self assert: 584 printStringRoman = 'DLXXXIV'.
	self assert: 585 printStringRoman = 'DLXXXV'.
	self assert: 586 printStringRoman = 'DLXXXVI'.
	self assert: 681 printStringRoman = 'DCLXXXI'.
	self assert: 682 printStringRoman = 'DCLXXXII'.
	self assert: 683 printStringRoman = 'DCLXXXIII'.
	self assert: 778 printStringRoman = 'DCCLXXVIII'.
	self assert: 779 printStringRoman = 'DCCLXXIX'.
	self assert: 780 printStringRoman = 'DCCLXXX'.
	self assert: 875 printStringRoman = 'DCCCLXXV'.
	self assert: 876 printStringRoman = 'DCCCLXXVI'.
	self assert: 877 printStringRoman = 'DCCCLXXVII'.
	self assert: 972 printStringRoman = 'CMLXXII'.
	self assert: 973 printStringRoman = 'CMLXXIII'.
	self assert: 974 printStringRoman = 'CMLXXIV'.
	self assert: 1069 printStringRoman = 'MLXIX'.
	self assert: 1070 printStringRoman = 'MLXX'.
	self assert: 1071 printStringRoman = 'MLXXI'.
	self assert: 1166 printStringRoman = 'MCLXVI'.
	self assert: 1167 printStringRoman = 'MCLXVII'.
	self assert: 1168 printStringRoman = 'MCLXVIII'.
	self assert: 1263 printStringRoman = 'MCCLXIII'.
	self assert: 1264 printStringRoman = 'MCCLXIV'.
	self assert: 1265 printStringRoman = 'MCCLXV'.
	self assert: 1360 printStringRoman = 'MCCCLX'.
	self assert: 1361 printStringRoman = 'MCCCLXI'.
	self assert: 1362 printStringRoman = 'MCCCLXII'.
	self assert: 1457 printStringRoman = 'MCDLVII'.
	self assert: 1458 printStringRoman = 'MCDLVIII'.
	self assert: 1459 printStringRoman = 'MCDLIX'.
	self assert: 1554 printStringRoman = 'MDLIV'.
	self assert: 1555 printStringRoman = 'MDLV'.
	self assert: 1556 printStringRoman = 'MDLVI'.
	self assert: 1651 printStringRoman = 'MDCLI'.
	self assert: 1652 printStringRoman = 'MDCLII'.
	self assert: 1653 printStringRoman = 'MDCLIII'.
	self assert: 1748 printStringRoman = 'MDCCXLVIII'.
	self assert: 1749 printStringRoman = 'MDCCXLIX'.
	self assert: 1750 printStringRoman = 'MDCCL'.
	self assert: 1845 printStringRoman = 'MDCCCXLV'.
	self assert: 1846 printStringRoman = 'MDCCCXLVI'.
	self assert: 1847 printStringRoman = 'MDCCCXLVII'.
	self assert: 1942 printStringRoman = 'MCMXLII'.
	self assert: 1943 printStringRoman = 'MCMXLIII'.
	self assert: 1944 printStringRoman = 'MCMXLIV'.
	self assert: 2004 printStringRoman = 'MMIV'.

	self assert: -1 printStringRoman = '-I'.
	self assert: -2 printStringRoman = '-II'.
	self assert: -3 printStringRoman = '-III'.
	self assert: -4 printStringRoman = '-IV'.
	self assert: -5 printStringRoman = '-V'.
	self assert: -6 printStringRoman = '-VI'.
	self assert: -7 printStringRoman = '-VII'.
	self assert: -8 printStringRoman = '-VIII'.
	self assert: -9 printStringRoman = '-IX'.
	self assert: -10 printStringRoman = '-X'.

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

	#(-1234567890123 -10 -5 -1) do: [ :each |
		self should: [ each sqrtFloor ] raise: Error ].
	#(
		0 1 2 3 4 5 10 16 30 160479924 386234481 501619156 524723498 580855366 766098594 834165249 1020363860 1042083924 1049218924
		1459774772895569 3050005981408238 4856589481837079 5650488387708463 7831037396100244) do: [ :each |
			self assert: each asFloat sqrt floor = each sqrtFloor ]
		
]
