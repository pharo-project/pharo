"
SUnit tests for integers.
"
Class {
	#name : #IntegerTest,
	#superclass : #TestCase,
	#category : #'Kernel-Tests-Numbers'
}

{ #category : #private }
IntegerTest >> assert: a classAndValueEquals: b [
	self assert: a class equals: b class.
	self assert: a equals: b
]

{ #category : #'tests - basic' }
IntegerTest >> testAsLargerPowerOfTwo [
"Invalid input testing"
	"LargeNegativeIntegers"
	self should: [(2 raisedTo: 80) negated asLargerPowerOfTwo] raise: DomainError.
	"Negative SmallIntegers"
	self should: [-1 asLargerPowerOfTwo] raise: DomainError.
	"0"
	self should: [0 asLargerPowerOfTwo] raise: DomainError.
"Valid inputs"
	"Small integers"
	self assert: 1 asLargerPowerOfTwo equals: 1.
	self assert: 2 asLargerPowerOfTwo equals: 2.
	self assert: 3 asLargerPowerOfTwo equals: 4.
	self assert: 4 asLargerPowerOfTwo equals: 4.
	self assert: 5 asLargerPowerOfTwo equals: 8.
	"Large integers"
	self assert: ((2 raisedTo: 80) +1) asLargerPowerOfTwo equals: (2 raisedTo: 80 +1).
	self assert: (2 raisedTo: 80) asLargerPowerOfTwo equals: (2 raisedTo: 80).
	self assert: ((2 raisedTo: 80) - 1) asLargerPowerOfTwo equals: (2 raisedTo: 80)
]

{ #category : #'tests - basic' }
IntegerTest >> testAsSmallerPowerOfTwo [
"Invalid input testing"
	"LargeNegativeIntegers"
	self should: [(2 raisedTo: 80) negated asSmallerPowerOfTwo] raise: DomainError.
	"Negative SmallIntegers"
	self should: [-1 asSmallerPowerOfTwo] raise: DomainError.
	"0"
	self should: [0 asSmallerPowerOfTwo] raise: DomainError.
"Valid inputs"
	"Small integers"
	self assert: 1 asSmallerPowerOfTwo equals: 1.
	self assert: 2 asSmallerPowerOfTwo equals: 2.
	self assert: 3 asSmallerPowerOfTwo equals: 2.
	self assert: 4 asSmallerPowerOfTwo equals: 4.
	self assert: 5 asSmallerPowerOfTwo equals: 4.
	"Large integers"
	self assert: ((2 raisedTo: 80) +1) asSmallerPowerOfTwo equals: (2 raisedTo: 80).
	self assert: (2 raisedTo: 80) asSmallerPowerOfTwo equals: (2 raisedTo: 80).
	self assert: ((2 raisedTo: 80) - 1) asSmallerPowerOfTwo equals: (2 raisedTo: 80 - 1)
]

{ #category : #'tests - printing' }
IntegerTest >> testAsStringWithCommas [
	self assert: 123456789 asStringWithCommas equals: '123,456,789'.
	self assert: -123456789 asStringWithCommas equals: '-123,456,789'
]

{ #category : #'tests - printing' }
IntegerTest >> testBadBase [
	"This used to get into an endless loop.
	See Pharo #114"
	
	self should: [2 printStringBase: 1] raise: Error.
]

{ #category : #'tests - bitLogic' }
IntegerTest >> testBitAnd [

	self assert: (2r1100 bitAnd: 2r1010) equals: 2r1000.
	self assert: (2r1100 & 2r1010) equals: 2r1000.
	self assert: (-2 bitAnd: 16rFFFFFFFF) equals: 16rFFFFFFFE.
	self assert: (-2 & 16rFFFFFFFF) equals: 16rFFFFFFFE.
]

{ #category : #'tests - bitLogic' }
IntegerTest >> testBitAt [
	| trials bitSequence2 |
	self assert: (2r10 bitAt: 1) equals: 0.
	self assert: (2r10 bitAt: 2) equals: 1.

	self assert: ((1 to: 100) allSatisfy: [ :i | (0 bitAt: i) = 0 ]) description: 'all bits of zero are set to zero'.

	self assert: ((1 to: 100) allSatisfy: [ :i | (-1 bitAt: i) = 1 ]) description: 'In two complements, all bits of -1 are set to 1'.


	trials := #('2r10010011' '2r11100100' '2r10000000' '2r0000101011011001' '2r1000101011011001' '2r0101010101011000' '2r0010011110110010' '2r0010011000000000' '2r00100111101100101000101011011001' '2r01110010011110110010100110101101' '2r10101011101011001010000010110110' '2r10101000000000000000000000000000' '2r0010101110101001110010100000101101100010011110110010100010101100' '2r1010101110101100101000001011011000100111101100101000101011011001' '2r1010101110101000000000000000000000000000000000000000000000000000').
	trials
		do: [ :bitSequence | 
			| aNumber |
			aNumber := Number readFrom: bitSequence.
			bitSequence2 := (bitSequence size - 2 to: 1 by: -1) inject: '2r' into: [ :string :i | string copyWith: (Character digitValue: (aNumber bitAt: i)) ].
			self assert: bitSequence2 equals: bitSequence ].

	trials
		do: [ :bitSequence | 
			| bitInvert |
			bitInvert := -1 - (Number readFrom: bitSequence).
			bitSequence2 := (bitSequence size - 2 to: 1 by: -1) inject: '2r' into: [ :string :i | string copyWith: (Character digitValue: 1 - (bitInvert bitAt: i)) ].
			self assert: bitSequence2 = bitSequence description: '-1-x is similar to a bitInvert operation in two complement' ]
]

{ #category : #'tests - bitLogic' }
IntegerTest >> testBitClear [

	self assert: (2r1111 bitClear: 2r1000) equals: 2r0111.
	self assert: (2r1111 bitClear: 2r0100) equals: 2r1011.
	self assert: (2r1111 bitClear: 2r0010) equals: 2r1101.
	self assert: (2r1111 bitClear: 2r0001) equals: 2r1110.
]

{ #category : #'tests - bitLogic' }
IntegerTest >> testBitLogic [  
	"This little suite of tests is designed to verify correct operation of most of Pharo's bit manipulation code, including two's complement representation of negative values.  It was written in a hurry and is probably lacking several important checks."
	
	| n |
	"Shift 1 bit left then right and test for 1"
	1 to: 100 do: [:i | self assert: ((1 bitShift: i) bitShift: i negated) equals: 1].

	"Shift -1 left then right and test for 1"
	1 to: 100 do: [:i | self assert: ((-1 bitShift: i) bitShift: i negated) equals: -1].

	"And a single bit with -1 and test for same value"
	1 to: 100 do: [:i | self assert: ((1 bitShift: i) bitAnd: -1) equals: (1 bitShift: i)].

	"Verify that (n bitAnd: n negated) = n for single bits"
	1 to: 100 do: [:i |  n := 1 bitShift: i. self assert: (n bitAnd: n negated) equals: n].

	"Verify that n negated = (n complemented + 1) for single bits"
	1 to: 100 do: [:i | 
					n := 1 bitShift: i. 
					self assert: n negated equals: ((n bitXor: -1) + 1)].

	"Verify that (n + n complemented) = -1 for single bits"
	1 to: 100 do: [:i | 
					n := 1 bitShift: i.
					self assert: (n + (n bitXor: -1)) equals: -1].

	"Verify that n negated = (n complemented +1) for single bits"
	1 to: 100 do: [:i | 
					n := 1 bitShift: i.
					self assert: n negated equals: ((n bitXor: -1) + 1)]
]

{ #category : #'tests - bitLogic' }
IntegerTest >> testBitMask [

	self assert: (2r11 allMask: 2r11).
	self deny: (2r10 allMask: 2r11).
	self deny: (2r01 allMask: 2r11).
	
	self assert: (2r10 anyMask: 2r11).
	self assert: (2r01 anyMask: 2r11).
	self deny: (2r00 anyMask: 2r11).
]

{ #category : #'tests - bitLogic' }
IntegerTest >> testBitShift [

	self assert: 2r11 << 2 equals: 2r1100.
	self assert: (2r11 bitShift: 2) equals: 2r1100.
	self assert: 2r1011 >> 2 equals: 2r10.
	self assert: (2r1011 bitShift: -2) equals: 2r10.
]

{ #category : #'tests - bitLogic' }
IntegerTest >> testBitString [
		
	Smalltalk vm wordSize = 4
		ifTrue: [
			self assert: 2 bitString equals: '0000000000000000000000000000010'.
			self assert: -1 bitString equals: '1111111111111111111111111111111'.
			self assert: -2 bitString equals: '1111111111111111111111111111110'.
			self assert: 2 bitStringLength equals: 31 ]. 
	Smalltalk vm wordSize = 8
		ifTrue: [
			self assert: 2 bitString equals: '0000000000000000000000000000000000000000000000000000000000010'.
			self assert: -1 bitString equals: '1111111111111111111111111111111111111111111111111111111111111'.
			self assert: -2 bitString equals: '1111111111111111111111111111111111111111111111111111111111110'.
			self assert: 2 bitStringLength equals: 61 ]. 
	"32 minus 1 for immediate encoding = 31 = 30 for number + 1 for sign"
	"64 minus 3 for immediate encoding = 61 = 60 for number + 1 for sign"
	self assert: 2 bitStringLength equals: (SmallInteger maxVal highBit + 1).
]

{ #category : #'tests - bitLogic' }
IntegerTest >> testBitXor [

	self assert: (2r1100 bitXor: 2r1010) equals: 2r0110.
]

{ #category : #'tests - instance creation' }
IntegerTest >> testCreationFromBytes1 [
	"It is illegal for a LargeInteger to be less than SmallInteger maxVal. Here we test that Integer>>byte!byte2:byte3:byte4: 
	 reconstructs SmallInteger maxVal as an instance of SmallInteger."
  
	| maxSmallInt hexString byte1 byte2 byte3 byte4 builtInteger |
	Smalltalk vm wordSize = 4 ifFalse: [ ^ self skip ].
	maxSmallInt := SmallInteger maxVal.
	hexString := maxSmallInt printStringHex.
	self assert: hexString size equals: 8.
	byte4 := Number readFrom: (hexString copyFrom: 1 to: 2) base: 16.
	byte3 := Number readFrom: (hexString copyFrom: 3 to: 4) base: 16.
	byte2 := Number readFrom: (hexString copyFrom: 5 to: 6) base: 16.
	byte1 := Number readFrom: (hexString copyFrom: 7 to: 8) base: 16.
	builtInteger := Integer byte1: byte1 byte2: byte2 byte3: byte3 byte4: byte4.
	self assert: builtInteger equals: maxSmallInt.
	self assert: builtInteger class equals: SmallInteger

]

{ #category : #'tests - instance creation' }
IntegerTest >> testCreationFromBytes2 [
	"It is illegal for a LargeInteger to be less than SmallInteger maxVal. Here we test that Integer>>byte!byte2:byte3:byte4: 
	 reconstructs (SmallInteger maxVal + 1) as an instance of LargePositiveInteger."

	| maxSmallInt hexString byte1 byte2 byte3 byte4 builtInteger |
	Smalltalk vm wordSize = 4 ifFalse: [ ^ self skip ].
	maxSmallInt := SmallInteger maxVal.
	hexString := (maxSmallInt + 1) printStringHex.
	self assert: hexString size equals: 8.
	byte4 := Number readFrom: (hexString copyFrom: 1 to: 2) base: 16.
	byte3 := Number readFrom: (hexString copyFrom: 3 to: 4) base: 16.
	byte2 := Number readFrom: (hexString copyFrom: 5 to: 6) base: 16.
	byte1 := Number readFrom: (hexString copyFrom: 7 to: 8) base: 16.
	builtInteger := Integer
		byte1: byte1
		byte2: byte2
		byte3: byte3
		byte4: byte4.
	self assert: builtInteger equals: maxSmallInt + 1.
	self deny: builtInteger class equals: SmallInteger
]

{ #category : #'tests - instance creation' }
IntegerTest >> testCreationFromBytes3 [
	"It is illegal for a LargeInteger to be less than SmallInteger maxVal. Here we test that Integer>>byte!byte2:byte3:byte4: 
	 reconstructs (SmallInteger maxVal - 1) as an instance of SmallInteger."
	
	| maxSmallInt hexString byte1 byte2 byte3 byte4 builtInteger |
	Smalltalk vm wordSize = 4 ifFalse: [ ^ self skip ].
	maxSmallInt := SmallInteger maxVal.
	hexString := (maxSmallInt - 1) printStringHex.
	self assert: hexString size equals: 8.
	byte4 := Number readFrom: (hexString copyFrom: 1 to: 2) base: 16.
	byte3 := Number readFrom: (hexString copyFrom: 3 to: 4) base: 16.
	byte2 := Number readFrom: (hexString copyFrom: 5 to: 6) base: 16.
	byte1 := Number readFrom: (hexString copyFrom: 7 to: 8) base: 16.
	builtInteger := Integer byte1: byte1 byte2: byte2 byte3: byte3 byte4: byte4.
	self assert: builtInteger equals: (maxSmallInt - 1).
	self assert: builtInteger class equals: SmallInteger

]

{ #category : #'tests - arithmetic' }
IntegerTest >> testCrossSumBase [

	self assert: ((-20 to: 20) collect: [:each | each crossSumBase: 10]) asArray   
		  equals: #(2 10 9 8 7 6 5 4 3 2 1 9 8 7 6 5 4 3 2 1 0 1 2 3 4 5 6 7 8 9 1 2 3 4 5 6 7 8 9 10 2).

	self assert: ((-20 to: 20) collect: [:each | each crossSumBase: 2]) asArray 
		  equals: #(2 3 2 2 1 4 3 3 2 3 2 2 1 3 2 2 1 2 1 1 0 1 1 2 1 2 2 3 1 2 2 3 2 3 3 4 1 2 2 3 2).

	self should: [ 10 crossSumBase: 1 ] raise: AssertionFailure
]

{ #category : #'tests - instance creation' }
IntegerTest >> testDifferentBases [
 
	"| value |
	2 to: 36 do: [:each|
		value := 0.
		1 to: each-1 do: [:n| value := value + (n * (each raisedToInteger: n))].
		value := value negated.
		Transcript tab; show: 'self assert: (', value printString, ' printStringBase: ', each printString, ') = ''', (value printStringBase: each), '''.'; cr.
		Transcript tab; show: 'self assert: (', value printString, ' radix: ', each printString, ') = ''', (value radix: each), '''.'; cr.
		Transcript tab; show: 'self assert: ', value printString, ' printStringHex = ''', (value printStringBase: 16), '''.'; cr.
		Transcript tab; show: 'self assert: (', value printString, ' storeStringBase: ', each printString, ') = ''', (value storeStringBase: each), '''.'; cr.
		Transcript tab; show: 'self assert: ', value printString, ' storeStringHex = ''', (value storeStringBase: 16), '''.'; cr.


].
	"

	self assert: 2r10 equals: 2.
	self assert: 3r210 equals: 21.
	self assert: 4r3210 equals: 228.
	self assert: 5r43210 equals: 2930.
	self assert: 6r543210 equals: 44790.
	self assert: 7r6543210 equals: 800667.
	self assert: 8r76543210 equals: 16434824.
	self assert: 9r876543210 equals: 381367044.
	self assert: 10r9876543210 equals: 9876543210.
	self assert: 11rA9876543210 equals: 282458553905.
	self assert: 12rBA9876543210 equals: 8842413667692.
	self assert: 13rCBA9876543210 equals: 300771807240918.
	self assert: 14rDCBA9876543210 equals: 11046255305880158.
	self assert: 15rEDCBA9876543210 equals: 435659737878916215.
	self assert: 16rFEDCBA9876543210 equals: 18364758544493064720.
	self assert: 17rGFEDCBA9876543210 equals: 824008854613343261192.
	self assert: 18rHGFEDCBA9876543210 equals: 39210261334551566857170.
	self assert: 19rIHGFEDCBA9876543210 equals: 1972313422155189164466189.
	self assert: 20rJIHGFEDCBA9876543210 equals: 104567135734072022160664820.
	self assert: 21rKJIHGFEDCBA9876543210 equals: 5827980550840017565077671610.
	self assert: 22rLKJIHGFEDCBA9876543210 equals: 340653664490377789692799452102.
	self assert: 23rMLKJIHGFEDCBA9876543210 equals: 20837326537038308910317109288851.
	self assert: 24rNMLKJIHGFEDCBA9876543210 equals: 1331214537196502869015340298036888.
	self assert: 25rONMLKJIHGFEDCBA9876543210 equals: 88663644327703473714387251271141900.
	self assert: 26rPONMLKJIHGFEDCBA9876543210 equals: 6146269788878825859099399609538763450.
	self assert: 27rQPONMLKJIHGFEDCBA9876543210 equals: 442770531899482980347734468443677777577.
	self assert: 28rRQPONMLKJIHGFEDCBA9876543210 equals: 33100056003358651440264672384704297711484.
	self assert: 29rSRQPONMLKJIHGFEDCBA9876543210 equals: 2564411043271974895869785066497940850811934.
	self assert: 30rTSRQPONMLKJIHGFEDCBA9876543210 equals: 205646315052919334126040428061831153388822830.
	self assert: 31rUTSRQPONMLKJIHGFEDCBA9876543210 equals: 17050208381689099029767742314582582184093573615.
	self assert: 32rVUTSRQPONMLKJIHGFEDCBA9876543210 equals: 1459980823972598128486511383358617792788444579872.
	self assert: 33rWVUTSRQPONMLKJIHGFEDCBA9876543210 equals: 128983956064237823710866404905431464703849549412368.
	self assert: 34rXWVUTSRQPONMLKJIHGFEDCBA9876543210 equals: 11745843093701610854378775891116314824081102660800418.
	self assert: 35rYXWVUTSRQPONMLKJIHGFEDCBA9876543210 equals: 1101553773143634726491620528194292510495517905608180485.
	self assert: 36rZYXWVUTSRQPONMLKJIHGFEDCBA9876543210 equals: 106300512100105327644605138221229898724869759421181854980.

	self assert: -2r10 equals: -2.
	self assert: -3r210 equals: -21.
	self assert: -4r3210 equals: -228.
	self assert: -5r43210 equals: -2930.
	self assert: -6r543210 equals: -44790.
	self assert: -7r6543210 equals: -800667.
	self assert: -8r76543210 equals: -16434824.
	self assert: -9r876543210 equals: -381367044.
	self assert: -10r9876543210 equals: -9876543210.
	self assert: -11rA9876543210 equals: -282458553905.
	self assert: -12rBA9876543210 equals: -8842413667692.
	self assert: -13rCBA9876543210 equals: -300771807240918.
	self assert: -14rDCBA9876543210 equals: -11046255305880158.
	self assert: -15rEDCBA9876543210 equals: -435659737878916215.
	self assert: -16rFEDCBA9876543210 equals: -18364758544493064720.
	self assert: -17rGFEDCBA9876543210 equals: -824008854613343261192.
	self assert: -18rHGFEDCBA9876543210 equals: -39210261334551566857170.
	self assert: -19rIHGFEDCBA9876543210 equals: -1972313422155189164466189.
	self assert: -20rJIHGFEDCBA9876543210 equals: -104567135734072022160664820.
	self assert: -21rKJIHGFEDCBA9876543210 equals: -5827980550840017565077671610.
	self assert: -22rLKJIHGFEDCBA9876543210 equals: -340653664490377789692799452102.
	self assert: -23rMLKJIHGFEDCBA9876543210 equals: -20837326537038308910317109288851.
	self assert: -24rNMLKJIHGFEDCBA9876543210 equals: -1331214537196502869015340298036888.
	self assert: -25rONMLKJIHGFEDCBA9876543210 equals: -88663644327703473714387251271141900.
	self assert: -26rPONMLKJIHGFEDCBA9876543210 equals: -6146269788878825859099399609538763450.
	self assert: -27rQPONMLKJIHGFEDCBA9876543210 equals: -442770531899482980347734468443677777577.
	self assert: -28rRQPONMLKJIHGFEDCBA9876543210 equals: -33100056003358651440264672384704297711484.
	self assert: -29rSRQPONMLKJIHGFEDCBA9876543210 equals: -2564411043271974895869785066497940850811934.
	self assert: -30rTSRQPONMLKJIHGFEDCBA9876543210 equals: -205646315052919334126040428061831153388822830.
	self assert: -31rUTSRQPONMLKJIHGFEDCBA9876543210 equals: -17050208381689099029767742314582582184093573615.
	self assert: -32rVUTSRQPONMLKJIHGFEDCBA9876543210 equals: -1459980823972598128486511383358617792788444579872.
	self assert: -33rWVUTSRQPONMLKJIHGFEDCBA9876543210 equals: -128983956064237823710866404905431464703849549412368.
	self assert: -34rXWVUTSRQPONMLKJIHGFEDCBA9876543210 equals: -11745843093701610854378775891116314824081102660800418.
	self assert: -35rYXWVUTSRQPONMLKJIHGFEDCBA9876543210 equals: -1101553773143634726491620528194292510495517905608180485.
	self assert: -36rZYXWVUTSRQPONMLKJIHGFEDCBA9876543210 equals: -106300512100105327644605138221229898724869759421181854980.
]

{ #category : #'tests - other' }
IntegerTest >> testDigitSum [

	self assert: 1234 digitSum equals: 10
]

{ #category : #'tests - other' }
IntegerTest >> testDigitSumOnNegative [

	self assert: -1234 digitSum equals: 10
]

{ #category : #'tests - basic' }
IntegerTest >> testEven [
	
	self deny: (1073741825 even).
	self assert: (1073741824  even).
	
]

{ #category : #'tests - printing' }
IntegerTest >> testHex [
	self assert: 0 hex equals: '16r0'.
	self assert: 12 hex equals: '16rC'.
	self assert: 1234 hex equals: '16r4D2'
]

{ #category : #'tests - bitLogic' }
IntegerTest >> testHighBit [
	| suite |
	self assert: 2r1110 highBit equals: 4.
	self assert: 2r0110 highBit equals: 3.
	self assert: 2r0000 highBit equals: 0.

	suite := (0 to: 1024) asArray , #(16rFDFD 16rFFFF 16r1000 16r1000000 16r1000001 16r70000000 16r7AFAFAFA) , {SmallInteger maxVal . (SmallInteger maxVal + 1)}.
	suite := suite , (suite collect: [ :e | e raisedTo: 20 ]).

	suite
		do: [ :anInteger | 
			| highBit shifted |
			highBit := 0.
			shifted := 1.
			[ shifted > anInteger ]
				whileFalse: [ highBit := highBit + 1.
					shifted := shifted bitShift: 1 ].
			self assert: anInteger highBit equals: highBit ]
]

{ #category : #'tests - bitLogic' }
IntegerTest >> testHighBitOfMagnitude [
	| suite |
	suite := (0 to: 1024) asArray , #(16rFDFD 16rFFFF 16r1000 16r1000000 16r1000001 16r70000000 16r7AFAFAFA) , {SmallInteger maxVal . (SmallInteger maxVal + 1)}.
	suite := suite , (suite collect: [ :e | e raisedTo: 20 ]).

	suite
		do: [ :anInteger | 
			| highBit shifted |
			highBit := 0.
			shifted := 1.
			[ shifted > anInteger ]
				whileFalse: [ highBit := highBit + 1.
					shifted := shifted bitShift: 1 ].
			self assert: anInteger highBitOfMagnitude equals: highBit.
			self assert: anInteger negated highBitOfMagnitude equals: highBit ]
]

{ #category : #'tests - printing' }
IntegerTest >> testIntegerHex [
	| result |
	result := 15 asInteger hex.
	self assert: result equals: '16rF'.
	result := 0 asInteger hex.
	self assert: result equals: '16r0'.
	result := 255 asInteger hex.
	self assert: result equals: '16rFF'.
	result := 90 asInteger hex.
	self assert: result equals: '16r5A'
]

{ #category : #'tests - printing' }
IntegerTest >> testIntegerPadding [
 
	self assert: (1 printStringBase: 10 length: 0 padded: false) equals: '1'.
	self assert: (1 printStringBase: 10 length: 1 padded: false) equals: '1'.
	self assert: (1 printStringBase: 10 length: 2 padded: false) equals: ' 1'.
	self assert: (1024 printStringBase: 10 length: 19 padded: false) equals: '               1024'.
	self assert: (1024 printStringBase: 10 length: -1 padded: false) equals: '1024'.
	self assert: (1024 printStringBase: 10 length: 5 padded: false) equals: ' 1024'.
	self assert: (-1024 printStringBase: 10 length: 5 padded: false) equals: '-1024'.
	self assert: (-1024 printStringBase: 10 length: 19 padded: false) equals: '              -1024'.

	self assert: (1 printStringBase: 10 length: 0 padded: true) equals: '1'.
	self assert: (1 printStringBase: 10 length: 1 padded: true) equals: '1'.
	self assert: (1 printStringBase: 10 length: 2 padded: true) equals: '01'.
	self assert: (1024 printStringBase: 10 length: 19 padded: true) equals: '0000000000000001024'.
	self assert: (1024 printStringBase: 10 length: -1 padded: true) equals: '1024'.
	self assert: (1024 printStringBase: 10 length: 5 padded: true) equals: '01024'.
	self assert: (-1024 printStringBase: 10 length: 5 padded: true) equals: '-1024'.
	self assert: (-1024 printStringBase: 10 length: 19 padded: true) equals: '-000000000000001024'.

	self assert: (1 printStringBase: 16 length: 0 padded: false) equals: '1'.
	self assert: (1 printStringBase: 16 length: 1 padded: false) equals: '1'.
	self assert: (1 printStringBase: 16 length: 2 padded: false) equals: ' 1'.
	self assert: (2047 printStringBase: 16 length: 19 padded: false) equals: '                7FF'.
	self assert: (2047 printStringBase: 16 length: -1 padded: false) equals: '7FF'.
	self assert: (2047 printStringBase: 16 length: 4 padded: false) equals: ' 7FF'.
	self assert: (-2047 printStringBase: 16 length: 4 padded: false) equals: '-7FF'.
	self assert: (-2047 printStringBase: 16 length: 19 padded: false) equals: '               -7FF'.

	self assert: (1 printStringBase: 16 length: 0 padded: true) equals: '1'.
	self assert: (1 printStringBase: 16 length: 1 padded: true) equals: '1'.
	self assert: (1 printStringBase: 16 length: 2 padded: true) equals: '01'.
	self assert: (2047 printStringBase: 16 length: 19 padded: true) equals: '00000000000000007FF'.
	self assert: (2047 printStringBase: 16 length: -1 padded: true) equals: '7FF'.
	self assert: (2047 printStringBase: 16 length: 4 padded: true) equals: '07FF'.
	self assert: (-2047 printStringBase: 16 length: 4 padded: true) equals: '-7FF'.
	self assert: (-2047 printStringBase: 16 length: 19 padded: true) equals: '-0000000000000007FF'.

	self assert: (1 storeStringBase: 10 length: 0 padded: false) equals: '1'.
	self assert: (1 storeStringBase: 10 length: 1 padded: false) equals: '1'.
	self assert: (1 storeStringBase: 10 length: 2 padded: false) equals: ' 1'.
	self assert: (1024 storeStringBase: 10 length: 19 padded: false) equals: '               1024'.
	self assert: (1024 storeStringBase: 10 length: -1 padded: false) equals: '1024'.
	self assert: (1024 storeStringBase: 10 length: 5 padded: false) equals:' 1024'.
	self assert: (-1024 storeStringBase: 10 length: 5 padded: false) equals: '-1024'.
	self assert: (-1024 storeStringBase: 10 length: 19 padded: false) equals: '              -1024'.

	self assert: (1 storeStringBase: 10 length: 0 padded: true) equals: '1'.
	self assert: (1 storeStringBase: 10 length: 1 padded: true) equals: '1'.
	self assert: (1 storeStringBase: 10 length: 2 padded: true) equals: '01'.
	self assert: (1024 storeStringBase: 10 length: 19 padded: true) equals: '0000000000000001024'.
	self assert: (1024 storeStringBase: 10 length: -1 padded: true) equals: '1024'.
	self assert: (1024 storeStringBase: 10 length: 5 padded: true) equals: '01024'.
	self assert: (-1024 storeStringBase: 10 length: 5 padded: true) equals: '-1024'.
	self assert: (-1024 storeStringBase: 10 length: 19 padded: true) equals: '-000000000000001024'.

	self assert: (1 storeStringBase: 16 length: 0 padded: false) equals: '16r1'.
	self assert: (1 storeStringBase: 16 length: 4 padded: false) equals: '16r1'.
	self assert: (1 storeStringBase: 16 length: 5 padded: false) equals: ' 16r1'.
	self assert: (2047 storeStringBase: 16 length: 19 padded: false) equals: '             16r7FF'.
	self assert: (2047 storeStringBase: 16 length: -1 padded: false) equals: '16r7FF'.
	self assert: (2047 storeStringBase: 16 length: 7 padded: false) equals: ' 16r7FF'.
	self assert: (-2047 storeStringBase: 16 length: 7 padded: false) equals: '-16r7FF'.
	self assert: (-2047 storeStringBase: 16 length: 19 padded: false) equals: '            -16r7FF'.

	self assert: (1 storeStringBase: 16 length: 0 padded: true) equals: '16r1'.
	self assert: (1 storeStringBase: 16 length: 4 padded: true) equals: '16r1'.
	self assert: (1 storeStringBase: 16 length: 5 padded: true) equals: '16r01'.
	self assert: (2047 storeStringBase: 16 length: 19 padded: true) equals: '16r00000000000007FF'.
	self assert: (2047 storeStringBase: 16 length: -1 padded: true) equals: '16r7FF'.
	self assert: (2047 storeStringBase: 16 length: 7 padded: true) equals: '16r07FF'.
	self assert: (-2047 storeStringBase: 16 length: 7 padded: true) equals: '-16r7FF'.
	self assert: (-2047 storeStringBase: 16 length: 19 padded: true) equals: '-16r0000000000007FF'

]

{ #category : #'tests - instance creation' }
IntegerTest >> testIntegerReadsNotOkFromStream [
	self should: [Integer readFrom: 'a23' readStream] raise: Error.
	self should: [Integer readFrom: '-a23' readStream] raise: Error.
	self should: [Integer readFrom: 'a3' readStream base: 8] raise: Error.
	self should: [Integer readFrom: '-a3' readStream base: 8] raise: Error.
]

{ #category : #'tests - instance creation' }
IntegerTest >> testIntegerReadsNotOkFromString [
	self should: [Integer readFrom: 'aaa'] raise: Error.
	self should: [Integer readFrom: '-aaa'] raise: Error.
	self should: [Integer readFrom: 'a3' base: 8] raise: Error.
]

{ #category : #'tests - instance creation' }
IntegerTest >> testIntegerReadsOkFromStream [
	self assert: (Integer readFrom: '123' readStream) equals: 123.
	self assert: (Integer readFrom: '-123' readStream) equals: -123.
	self assert: (Integer readFrom: 'a3' readStream base: 16) equals: 163.
	self assert: (Integer readFrom: '-a3' readStream base: 16) equals: -163.

	self assert: (Integer readFrom: '3a' readStream base: 10) equals: 3
]

{ #category : #'tests - instance creation' }
IntegerTest >> testIntegerReadsOkFromString [
	self assert: (Integer readFrom: '123') equals: 123.
	self assert: (Integer readFrom: '-123') equals: -123.
	self assert: (Integer readFrom: 'a3' base: 16) equals: 163.
	self assert: (Integer readFrom: '-a3' base: 16) equals: -163
]

{ #category : #'tests - other' }
IntegerTest >> testIsAbstract [ 

	self assert: Integer isAbstract 
]

{ #category : #'tests - basic' }
IntegerTest >> testIsInteger [

	self assert: (0 isInteger).
	
]

{ #category : #'tests - basic' }
IntegerTest >> testIsPowerOfTwo [
	"LargeNegativeIntegers"
	self deny: (2 raisedTo: 80) negated isPowerOfTwo.
	"Negative SmallIntegers"
	self deny: (-1 isPowerOfTwo).
	" 0, incase implementation has forgotten edge case"
	self deny: (0 isPowerOfTwo).
	"Positive SmallIntegers"
	self assert: (1 isPowerOfTwo).
	self assert: (2 isPowerOfTwo).
	self deny:  (3 isPowerOfTwo).
	self assert: (4 isPowerOfTwo).
	self deny: (5 isPowerOfTwo).
	" LargePositiveIntegers"
	self deny: ((2 raisedTo: 80) - 1) isPowerOfTwo.
	self assert: (2 raisedTo: 80) isPowerOfTwo.
	self deny: ((2 raisedTo: 80) + 1) isPowerOfTwo
]

{ #category : #'tests - basic' }
IntegerTest >> testIsPowerOfTwoM6873 [
	"This is a non regression test for http://bugs.squeak.org/view.php?id=6873"

	self deny: ((1 to: 80) anySatisfy: [:n | (2 raisedTo: n) negated isPowerOfTwo])
		description: 'A negative integer cannot be a power of two'.
]

{ #category : #'tests - bitLogic' }
IntegerTest >> testLowBit [
	| suite |
	"Simple examples"
	self assert: 2r1011 lowBit equals: 1.
	self assert: 2r1010 lowBit equals: 2.
	self assert: 2r000000 lowBit equals: 0.

	suite := (0 to: 1024) asArray , #(16rFDFD 16rFFFF 16r1000 16r1000000 16r1000001 16r70000000 16r7AFAFAFA) , {SmallInteger maxVal . (SmallInteger maxVal + 1)}.
	suite := suite , (suite collect: [ :e | e raisedTo: 20 ]).

	suite
		do: [ :anInteger | 
			| lowBit |
			lowBit := (anInteger respondsTo: #bitAt:)
				ifTrue: [ (1 to: anInteger highBit) detect: [ :bitIndex | (anInteger bitAt: bitIndex) ~= 0 ] ifNone: [ 0 ] ]
				ifFalse: [ (1 to: anInteger highBit) detect: [ :bitIndex | (anInteger bitAnd: (1 bitShift: bitIndex - 1)) ~= 0 ] ifNone: [ 0 ] ].
			self assert: anInteger lowBit equals: lowBit.
			self assert: anInteger negated lowBit equals: lowBit ]
]

{ #category : #'tests - printing' }
IntegerTest >> testNegativeIntegerPrinting [
 

	self assert: (-2 printStringBase: 2) equals: '-10'.
	self assert: (-2 radix: 2) equals: '-10'.
	self assert: -2 printStringHex equals: '-2'.
	self assert: (-2 storeStringBase: 2) equals: '-2r10'.
	self assert: -2 storeStringHex equals: '-16r2'.
	self assert: (-21 printStringBase: 3) equals: '-210'.
	self assert: (-21 radix: 3) equals: '-210'.
	self assert: -21 printStringHex equals: '-15'.
	self assert: (-21 storeStringBase: 3) equals: '-3r210'.
	self assert: -21 storeStringHex equals: '-16r15'.
	self assert: (-228 printStringBase: 4) equals: '-3210'.
	self assert: (-228 radix: 4) equals: '-3210'.
	self assert: -228 printStringHex equals: '-E4'.
	self assert: (-228 storeStringBase: 4) equals: '-4r3210'.
	self assert: -228 storeStringHex equals: '-16rE4'.
	self assert: (-2930 printStringBase: 5) equals: '-43210'.
	self assert: (-2930 radix: 5) equals: '-43210'.
	self assert: -2930 printStringHex equals: '-B72'.
	self assert: (-2930 storeStringBase: 5) equals: '-5r43210'.
	self assert: -2930 storeStringHex equals: '-16rB72'.
	self assert: (-44790 printStringBase: 6) equals: '-543210'.
	self assert: (-44790 radix: 6) equals: '-543210'.
	self assert: -44790 printStringHex equals: '-AEF6'.
	self assert: (-44790 storeStringBase: 6) equals: '-6r543210'.
	self assert: -44790 storeStringHex equals: '-16rAEF6'.
	self assert: (-800667 printStringBase: 7) equals: '-6543210'.
	self assert: (-800667 radix: 7) equals: '-6543210'.
	self assert: -800667 printStringHex equals: '-C379B'.
	self assert: (-800667 storeStringBase: 7) equals: '-7r6543210'.
	self assert: -800667 storeStringHex equals: '-16rC379B'.
	self assert: (-16434824 printStringBase: 8) equals: '-76543210'.
	self assert: (-16434824 radix: 8) equals: '-76543210'.
	self assert: -16434824 printStringHex equals: '-FAC688'.
	self assert: (-16434824 storeStringBase: 8) equals: '-8r76543210'.
	self assert: -16434824 storeStringHex equals: '-16rFAC688'.
	self assert: (-381367044 printStringBase: 9) equals: '-876543210'.
	self assert: (-381367044 radix: 9) equals: '-876543210'.
	self assert: -381367044 printStringHex equals: '-16BB3304'.
	self assert: (-381367044 storeStringBase: 9) equals: '-9r876543210'.
	self assert: -381367044 storeStringHex equals: '-16r16BB3304'.
	self assert: (-9876543210 printStringBase: 10) equals: '-9876543210'.
	self assert: (-9876543210 radix: 10) equals: '-9876543210'.
	self assert: -9876543210 printStringHex equals: '-24CB016EA'.
	self assert: (-9876543210 storeStringBase: 10) equals: '-9876543210'.
	self assert: -9876543210 storeStringHex equals: '-16r24CB016EA'.
	self assert: (-282458553905 printStringBase: 11) equals: '-A9876543210'.
	self assert: (-282458553905 radix: 11) equals: '-A9876543210'.
	self assert: -282458553905 printStringHex equals: '-41C3D77E31'.
	self assert: (-282458553905 storeStringBase: 11) equals: '-11rA9876543210'.
	self assert: -282458553905 storeStringHex equals: '-16r41C3D77E31'.
	self assert: (-8842413667692 printStringBase: 12) equals: '-BA9876543210'.
	self assert: (-8842413667692 radix: 12) equals: '-BA9876543210'.
	self assert: -8842413667692 printStringHex equals: '-80AC8ECF56C'.
	self assert: (-8842413667692 storeStringBase: 12) equals: '-12rBA9876543210'.
	self assert: -8842413667692 storeStringHex equals: '-16r80AC8ECF56C'.
	self assert: (-300771807240918 printStringBase: 13) equals: '-CBA9876543210'.
	self assert: (-300771807240918 radix: 13) equals: '-CBA9876543210'.
	self assert: -300771807240918 printStringHex equals: '-1118CE4BAA2D6'.
	self assert: (-300771807240918 storeStringBase: 13) equals: '-13rCBA9876543210'.
	self assert: -300771807240918 storeStringHex equals: '-16r1118CE4BAA2D6'.
	self assert: (-11046255305880158 printStringBase: 14) equals: '-DCBA9876543210'.
	self assert: (-11046255305880158 radix: 14) equals: '-DCBA9876543210'.
	self assert: -11046255305880158 printStringHex equals: '-273E82BB9AF25E'.
	self assert: (-11046255305880158 storeStringBase: 14) equals: '-14rDCBA9876543210'.
	self assert: -11046255305880158 storeStringHex equals: '-16r273E82BB9AF25E'.
	self assert: (-435659737878916215 printStringBase: 15) equals: '-EDCBA9876543210'.
	self assert: (-435659737878916215 radix: 15) equals: '-EDCBA9876543210'.
	self assert: -435659737878916215 printStringHex equals: '-60BC6392F366C77'.
	self assert: (-435659737878916215 storeStringBase: 15) equals: '-15rEDCBA9876543210'.
	self assert: -435659737878916215 storeStringHex equals: '-16r60BC6392F366C77'.
	self assert: (-18364758544493064720 printStringBase: 16) equals: '-FEDCBA9876543210'.
	self assert: (-18364758544493064720 radix: 16) equals: '-FEDCBA9876543210'.
	self assert: -18364758544493064720 printStringHex equals: '-FEDCBA9876543210'.
	self assert: (-18364758544493064720 storeStringBase: 16) equals: '-16rFEDCBA9876543210'.
	self assert: -18364758544493064720 storeStringHex equals: '-16rFEDCBA9876543210'.
	self assert: (-824008854613343261192 printStringBase: 17) equals: '-GFEDCBA9876543210'.
	self assert: (-824008854613343261192 radix: 17) equals: '-GFEDCBA9876543210'.
	self assert: -824008854613343261192 printStringHex equals: '-2CAB6B877C1CD2D208'.
	self assert: (-824008854613343261192 storeStringBase: 17) equals: '-17rGFEDCBA9876543210'.
	self assert: -824008854613343261192 storeStringHex equals: '-16r2CAB6B877C1CD2D208'.
	self assert: (-39210261334551566857170 printStringBase: 18) equals: '-HGFEDCBA9876543210'.
	self assert: (-39210261334551566857170 radix: 18) equals: '-HGFEDCBA9876543210'.
	self assert: -39210261334551566857170 printStringHex equals: '-84D97AFCAE81415B3D2'.
	self assert: (-39210261334551566857170 storeStringBase: 18) equals: '-18rHGFEDCBA9876543210'.
	self assert: -39210261334551566857170 storeStringHex equals: '-16r84D97AFCAE81415B3D2'.
	self assert: (-1972313422155189164466189 printStringBase: 19) equals: '-IHGFEDCBA9876543210'.
	self assert: (-1972313422155189164466189 radix: 19) equals: '-IHGFEDCBA9876543210'.
	self assert: -1972313422155189164466189 printStringHex equals: '-1A1A75329C5C6FC00600D'.
	self assert: (-1972313422155189164466189 storeStringBase: 19) equals: '-19rIHGFEDCBA9876543210'.
	self assert: -1972313422155189164466189 storeStringHex equals: '-16r1A1A75329C5C6FC00600D'.
	self assert: (-104567135734072022160664820 printStringBase: 20) equals: '-JIHGFEDCBA9876543210'.
	self assert: (-104567135734072022160664820 radix: 20) equals: '-JIHGFEDCBA9876543210'.
	self assert: -104567135734072022160664820 printStringHex equals: '-567EF3C9636D242A8C68F4'.
	self assert: (-104567135734072022160664820 storeStringBase: 20) equals: '-20rJIHGFEDCBA9876543210'.
	self assert: -104567135734072022160664820 storeStringHex equals: '-16r567EF3C9636D242A8C68F4'.
	self assert: (-5827980550840017565077671610 printStringBase: 21) equals: '-KJIHGFEDCBA9876543210'.
	self assert: (-5827980550840017565077671610 radix: 21) equals: '-KJIHGFEDCBA9876543210'.
	self assert: -5827980550840017565077671610 printStringHex equals: '-12D4CAE2B8A09BCFDBE30EBA'.
	self assert: (-5827980550840017565077671610 storeStringBase: 21) equals: '-21rKJIHGFEDCBA9876543210'.
	self assert: -5827980550840017565077671610 storeStringHex equals: '-16r12D4CAE2B8A09BCFDBE30EBA'.
	self assert: (-340653664490377789692799452102 printStringBase: 22) equals: '-LKJIHGFEDCBA9876543210'.
	self assert: (-340653664490377789692799452102 radix: 22) equals: '-LKJIHGFEDCBA9876543210'.
	self assert: -340653664490377789692799452102 printStringHex equals: '-44CB61B5B47E1A5D8F88583C6'.
	self assert: (-340653664490377789692799452102 storeStringBase: 22) equals: '-22rLKJIHGFEDCBA9876543210'.
	self assert: -340653664490377789692799452102 storeStringHex equals: '-16r44CB61B5B47E1A5D8F88583C6'.
	self assert: (-20837326537038308910317109288851 printStringBase: 23) equals: '-MLKJIHGFEDCBA9876543210'.
	self assert: (-20837326537038308910317109288851 radix: 23) equals: '-MLKJIHGFEDCBA9876543210'.
	self assert: -20837326537038308910317109288851 printStringHex equals: '-1070108876456E0EF115B389F93'.
	self assert: (-20837326537038308910317109288851 storeStringBase: 23) equals: '-23rMLKJIHGFEDCBA9876543210'.
	self assert: -20837326537038308910317109288851 storeStringHex equals: '-16r1070108876456E0EF115B389F93'.
	self assert: (-1331214537196502869015340298036888 printStringBase: 24) equals: '-NMLKJIHGFEDCBA9876543210'.
	self assert: (-1331214537196502869015340298036888 radix: 24) equals: '-NMLKJIHGFEDCBA9876543210'.
	self assert: -1331214537196502869015340298036888 printStringHex equals: '-41A24A285154B026B6ED206C6698'.
	self assert: (-1331214537196502869015340298036888 storeStringBase: 24) equals: '-24rNMLKJIHGFEDCBA9876543210'.
	self assert: -1331214537196502869015340298036888 storeStringHex equals: '-16r41A24A285154B026B6ED206C6698'.
	self assert: (-88663644327703473714387251271141900 printStringBase: 25) equals: '-ONMLKJIHGFEDCBA9876543210'.
	self assert: (-88663644327703473714387251271141900 radix: 25) equals: '-ONMLKJIHGFEDCBA9876543210'.
	self assert: -88663644327703473714387251271141900 printStringHex equals: '-111374860A2C6CEBE5999630398A0C'.
	self assert: (-88663644327703473714387251271141900 storeStringBase: 25) equals: '-25rONMLKJIHGFEDCBA9876543210'.
	self assert: -88663644327703473714387251271141900 storeStringHex equals: '-16r111374860A2C6CEBE5999630398A0C'.
	self assert: (-6146269788878825859099399609538763450 printStringBase: 26) equals: '-PONMLKJIHGFEDCBA9876543210'.
	self assert: (-6146269788878825859099399609538763450 radix: 26) equals: '-PONMLKJIHGFEDCBA9876543210'.
	self assert: -6146269788878825859099399609538763450 printStringHex equals: '-49FBA7F30B0F48BD14E6A99BD8ADABA'.
	self assert: (-6146269788878825859099399609538763450 storeStringBase: 26) equals: '-26rPONMLKJIHGFEDCBA9876543210'.
	self assert: -6146269788878825859099399609538763450 storeStringHex equals: '-16r49FBA7F30B0F48BD14E6A99BD8ADABA'.
	self assert: (-442770531899482980347734468443677777577 printStringBase: 27) equals: '-QPONMLKJIHGFEDCBA9876543210'.
	self assert: (-442770531899482980347734468443677777577 radix: 27) equals: '-QPONMLKJIHGFEDCBA9876543210'.
	self assert: -442770531899482980347734468443677777577 printStringHex equals: '-14D1A80A997343640C1145A073731DEA9'.
	self assert: (-442770531899482980347734468443677777577 storeStringBase: 27) equals: '-27rQPONMLKJIHGFEDCBA9876543210'.
	self assert: -442770531899482980347734468443677777577 storeStringHex equals: '-16r14D1A80A997343640C1145A073731DEA9'.
	self assert: (-33100056003358651440264672384704297711484 printStringBase: 28) equals: '-RQPONMLKJIHGFEDCBA9876543210'.
	self assert: (-33100056003358651440264672384704297711484 radix: 28) equals: '-RQPONMLKJIHGFEDCBA9876543210'.
	self assert: -33100056003358651440264672384704297711484 printStringHex equals: '-6145B6E6DACFA25D0E936F51D25932377C'.
	self assert: (-33100056003358651440264672384704297711484 storeStringBase: 28) equals: '-28rRQPONMLKJIHGFEDCBA9876543210'.
	self assert: -33100056003358651440264672384704297711484 storeStringHex equals: '-16r6145B6E6DACFA25D0E936F51D25932377C'.
	self assert: (-2564411043271974895869785066497940850811934 printStringBase: 29) equals: '-SRQPONMLKJIHGFEDCBA9876543210'.
	self assert: (-2564411043271974895869785066497940850811934 radix: 29) equals: '-SRQPONMLKJIHGFEDCBA9876543210'.
	self assert: -2564411043271974895869785066497940850811934 printStringHex equals: '-1D702071CBA4A1597D4DD37E95EFAC79241E'.
	self assert: (-2564411043271974895869785066497940850811934 storeStringBase: 29) equals: '-29rSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: -2564411043271974895869785066497940850811934 storeStringHex equals: '-16r1D702071CBA4A1597D4DD37E95EFAC79241E'.
	self assert: (-205646315052919334126040428061831153388822830 printStringBase: 30) equals: '-TSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: (-205646315052919334126040428061831153388822830 radix: 30) equals: '-TSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: -205646315052919334126040428061831153388822830 printStringHex equals: '-938B4343B54B550989989D02998718FFB212E'.
	self assert: (-205646315052919334126040428061831153388822830 storeStringBase: 30) equals: '-30rTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: -205646315052919334126040428061831153388822830 storeStringHex equals: '-16r938B4343B54B550989989D02998718FFB212E'.
	self assert: (-17050208381689099029767742314582582184093573615 printStringBase: 31) equals: '-UTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: (-17050208381689099029767742314582582184093573615 radix: 31) equals: '-UTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: -17050208381689099029767742314582582184093573615 printStringHex equals: '-2FC8ECB1521BA16D24A69E976D53873E2C661EF'.
	self assert: (-17050208381689099029767742314582582184093573615 storeStringBase: 31) equals: '-31rUTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: -17050208381689099029767742314582582184093573615 storeStringHex equals: '-16r2FC8ECB1521BA16D24A69E976D53873E2C661EF'.
	self assert: (-1459980823972598128486511383358617792788444579872 printStringBase: 32) equals: '-VUTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: (-1459980823972598128486511383358617792788444579872 radix: 32) equals: '-VUTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: -1459980823972598128486511383358617792788444579872 printStringHex equals: '-FFBBCDEB38BDAB49CA307B9AC5A928398A418820'.
	self assert: (-1459980823972598128486511383358617792788444579872 storeStringBase: 32) equals: '-32rVUTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: -1459980823972598128486511383358617792788444579872 storeStringHex equals: '-16rFFBBCDEB38BDAB49CA307B9AC5A928398A418820'.
	self assert: (-128983956064237823710866404905431464703849549412368 printStringBase: 33) equals: '-WVUTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: (-128983956064237823710866404905431464703849549412368 radix: 33) equals: '-WVUTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: -128983956064237823710866404905431464703849549412368 printStringHex equals: '-584120A0328DE272AB055A8AA003CE4A559F223810'.
	self assert: (-128983956064237823710866404905431464703849549412368 storeStringBase: 33) equals: '-33rWVUTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: -128983956064237823710866404905431464703849549412368 storeStringHex equals: '-16r584120A0328DE272AB055A8AA003CE4A559F223810'.
	self assert: (-11745843093701610854378775891116314824081102660800418 printStringBase: 34) equals: '-XWVUTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: (-11745843093701610854378775891116314824081102660800418 radix: 34) equals: '-XWVUTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: -11745843093701610854378775891116314824081102660800418 printStringHex equals: '-1F64D4FC76000F7B92CF0CD5D0F350139AB9F25D8FA2'.
	self assert: (-11745843093701610854378775891116314824081102660800418 storeStringBase: 34) equals: '-34rXWVUTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: -11745843093701610854378775891116314824081102660800418 storeStringHex equals: '-16r1F64D4FC76000F7B92CF0CD5D0F350139AB9F25D8FA2'.
	self assert: (-1101553773143634726491620528194292510495517905608180485 printStringBase: 35) equals: '-YXWVUTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: (-1101553773143634726491620528194292510495517905608180485 radix: 35) equals: '-YXWVUTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: -1101553773143634726491620528194292510495517905608180485 printStringHex equals: '-B8031AD55AD1FAA89E07A271CA1ED2F420415D1570305'.
	self assert: (-1101553773143634726491620528194292510495517905608180485 storeStringBase: 35) equals: '-35rYXWVUTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: -1101553773143634726491620528194292510495517905608180485 storeStringHex equals: '-16rB8031AD55AD1FAA89E07A271CA1ED2F420415D1570305'.
	self assert: (-106300512100105327644605138221229898724869759421181854980 printStringBase: 36) equals: '-ZYXWVUTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: (-106300512100105327644605138221229898724869759421181854980 radix: 36) equals: '-ZYXWVUTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: -106300512100105327644605138221229898724869759421181854980 printStringHex equals: '-455D441E55A37239AB4C303189576071AF5578FFCA80504'.
	self assert: (-106300512100105327644605138221229898724869759421181854980 storeStringBase: 36) equals: '-36rZYXWVUTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: -106300512100105327644605138221229898724869759421181854980 storeStringHex equals: '-16r455D441E55A37239AB4C303189576071AF5578FFCA80504'.
]

{ #category : #'tests - instance creation' }
IntegerTest >> testNew [
	self should: [Integer new] raise: self defaultTestError. 
]

{ #category : #'tests - other' }
IntegerTest >> testNotInstantiable [

	self should: [ Integer new ] raise: Exception
]

{ #category : #'tests - printing' }
IntegerTest >> testNumberOfDigits [
	2 to: 32 do: [ :b | 
		1 to: 1000 // b do: [ :n | 
			| bRaisedToN |
			bRaisedToN := b raisedTo: n.
			self assert: (bRaisedToN - 1 numberOfDigitsInBase: b) equals: n.
			self assert: (bRaisedToN numberOfDigitsInBase: b) equals: n + 1.
			self assert: (bRaisedToN + 1 numberOfDigitsInBase: b) equals: n + 1.

			self assert: (bRaisedToN negated + 1 numberOfDigitsInBase: b) equals: n.
			self assert: (bRaisedToN negated numberOfDigitsInBase: b) equals: n + 1.
			self assert: (bRaisedToN negated - 1 numberOfDigitsInBase: b) equals: n + 1 ] ]
]

{ #category : #'tests - printing' }
IntegerTest >> testPositiveIntegerPrinting [

	self assert: 0 printString equals: '0'.
	self assert: 0 printStringHex equals: '0'.
	self assert: 0 storeStringHex equals: '16r0'.

	self assert: (2 printStringBase: 2) equals: '10'.
	self assert: (2 radix: 2) equals: '10'.
	self assert: 2 printStringHex equals: '2'.
	self assert: (2 storeStringBase: 2) equals: '2r10'.
	self assert: 2 storeStringHex equals: '16r2'.
	self assert: (21 printStringBase: 3) equals: '210'.
	self assert: (21 radix: 3) equals: '210'.
	self assert: 21 printStringHex equals: '15'.
	self assert: (21 storeStringBase: 3) equals: '3r210'.
	self assert: 21 storeStringHex equals: '16r15'.
	self assert: (228 printStringBase: 4) equals: '3210'.
	self assert: (228 radix: 4) equals: '3210'.
	self assert: 228 printStringHex equals: 'E4'.
	self assert: (228 storeStringBase: 4) equals: '4r3210'.
	self assert: 228 storeStringHex equals: '16rE4'.
	self assert: (2930 printStringBase: 5) equals: '43210'.
	self assert: (2930 radix: 5) equals: '43210'.
	self assert: 2930 printStringHex equals: 'B72'.
	self assert: (2930 storeStringBase: 5) equals: '5r43210'.
	self assert: 2930 storeStringHex equals: '16rB72'.
	self assert: (44790 printStringBase: 6) equals: '543210'.
	self assert: (44790 radix: 6) equals: '543210'.
	self assert: 44790 printStringHex equals: 'AEF6'.
	self assert: (44790 storeStringBase: 6) equals: '6r543210'.
	self assert: 44790 storeStringHex equals: '16rAEF6'.
	self assert: (800667 printStringBase: 7) equals: '6543210'.
	self assert: (800667 radix: 7) equals: '6543210'.
	self assert: 800667 printStringHex equals: 'C379B'.
	self assert: (800667 storeStringBase: 7) equals: '7r6543210'.
	self assert: 800667 storeStringHex equals: '16rC379B'.
	self assert: (16434824 printStringBase: 8) equals: '76543210'.
	self assert: (16434824 radix: 8) equals: '76543210'.
	self assert: 16434824 printStringHex equals: 'FAC688'.
	self assert: (16434824 storeStringBase: 8) equals: '8r76543210'.
	self assert: 16434824 storeStringHex equals: '16rFAC688'.
	self assert: (381367044 printStringBase: 9) equals: '876543210'.
	self assert: (381367044 radix: 9) equals: '876543210'.
	self assert: 381367044 printStringHex equals: '16BB3304'.
	self assert: (381367044 storeStringBase: 9) equals: '9r876543210'.
	self assert: 381367044 storeStringHex equals: '16r16BB3304'.
	self assert: (9876543210 printStringBase: 10) equals: '9876543210'.
	self assert: (9876543210 radix: 10) equals: '9876543210'.
	self assert: 9876543210 printStringHex equals: '24CB016EA'.
	self assert: (9876543210 storeStringBase: 10) equals: '9876543210'.
	self assert: 9876543210 storeStringHex equals: '16r24CB016EA'.
	self assert: (282458553905 printStringBase: 11) equals: 'A9876543210'.
	self assert: (282458553905 radix: 11) equals: 'A9876543210'.
	self assert: 282458553905 printStringHex equals: '41C3D77E31'.
	self assert: (282458553905 storeStringBase: 11) equals: '11rA9876543210'.
	self assert: 282458553905 storeStringHex equals: '16r41C3D77E31'.
	self assert: (8842413667692 printStringBase: 12) equals: 'BA9876543210'.
	self assert: (8842413667692 radix: 12) equals: 'BA9876543210'.
	self assert: 8842413667692 printStringHex equals: '80AC8ECF56C'.
	self assert: (8842413667692 storeStringBase: 12) equals: '12rBA9876543210'.
	self assert: 8842413667692 storeStringHex equals: '16r80AC8ECF56C'.
	self assert: (300771807240918 printStringBase: 13) equals: 'CBA9876543210'.
	self assert: (300771807240918 radix: 13) equals: 'CBA9876543210'.
	self assert: 300771807240918 printStringHex equals: '1118CE4BAA2D6'.
	self assert: (300771807240918 storeStringBase: 13) equals: '13rCBA9876543210'.
	self assert: 300771807240918 storeStringHex equals: '16r1118CE4BAA2D6'.
	self assert: (11046255305880158 printStringBase: 14) equals: 'DCBA9876543210'.
	self assert: (11046255305880158 radix: 14) equals: 'DCBA9876543210'.
	self assert: 11046255305880158 printStringHex equals: '273E82BB9AF25E'.
	self assert: (11046255305880158 storeStringBase: 14) equals: '14rDCBA9876543210'.
	self assert: 11046255305880158 storeStringHex equals: '16r273E82BB9AF25E'.
	self assert: (435659737878916215 printStringBase: 15) equals: 'EDCBA9876543210'.
	self assert: (435659737878916215 radix: 15) equals: 'EDCBA9876543210'.
	self assert: 435659737878916215 printStringHex equals: '60BC6392F366C77'.
	self assert: (435659737878916215 storeStringBase: 15) equals: '15rEDCBA9876543210'.
	self assert: 435659737878916215 storeStringHex equals: '16r60BC6392F366C77'.
	self assert: (18364758544493064720 printStringBase: 16) equals: 'FEDCBA9876543210'.
	self assert: (18364758544493064720 radix: 16) equals: 'FEDCBA9876543210'.
	self assert: 18364758544493064720 printStringHex equals: 'FEDCBA9876543210'.
	self assert: (18364758544493064720 storeStringBase: 16) equals: '16rFEDCBA9876543210'.
	self assert: 18364758544493064720 storeStringHex equals: '16rFEDCBA9876543210'.
	self assert: (824008854613343261192 printStringBase: 17) equals: 'GFEDCBA9876543210'.
	self assert: (824008854613343261192 radix: 17) equals: 'GFEDCBA9876543210'.
	self assert: 824008854613343261192 printStringHex equals: '2CAB6B877C1CD2D208'.
	self assert: (824008854613343261192 storeStringBase: 17) equals: '17rGFEDCBA9876543210'.
	self assert: 824008854613343261192 storeStringHex equals: '16r2CAB6B877C1CD2D208'.
	self assert: (39210261334551566857170 printStringBase: 18) equals: 'HGFEDCBA9876543210'.
	self assert: (39210261334551566857170 radix: 18) equals: 'HGFEDCBA9876543210'.
	self assert: 39210261334551566857170 printStringHex equals: '84D97AFCAE81415B3D2'.
	self assert: (39210261334551566857170 storeStringBase: 18) equals: '18rHGFEDCBA9876543210'.
	self assert: 39210261334551566857170 storeStringHex equals: '16r84D97AFCAE81415B3D2'.
	self assert: (1972313422155189164466189 printStringBase: 19) equals: 'IHGFEDCBA9876543210'.
	self assert: (1972313422155189164466189 radix: 19) equals: 'IHGFEDCBA9876543210'.
	self assert: 1972313422155189164466189 printStringHex equals: '1A1A75329C5C6FC00600D'.
	self assert: (1972313422155189164466189 storeStringBase: 19) equals: '19rIHGFEDCBA9876543210'.
	self assert: 1972313422155189164466189 storeStringHex equals: '16r1A1A75329C5C6FC00600D'.
	self assert: (104567135734072022160664820 printStringBase: 20) equals: 'JIHGFEDCBA9876543210'.
	self assert: (104567135734072022160664820 radix: 20) equals: 'JIHGFEDCBA9876543210'.
	self assert: 104567135734072022160664820 printStringHex equals: '567EF3C9636D242A8C68F4'.
	self assert: (104567135734072022160664820 storeStringBase: 20) equals: '20rJIHGFEDCBA9876543210'.
	self assert: 104567135734072022160664820 storeStringHex equals: '16r567EF3C9636D242A8C68F4'.
	self assert: (5827980550840017565077671610 printStringBase: 21) equals: 'KJIHGFEDCBA9876543210'.
	self assert: (5827980550840017565077671610 radix: 21) equals: 'KJIHGFEDCBA9876543210'.
	self assert: 5827980550840017565077671610 printStringHex equals: '12D4CAE2B8A09BCFDBE30EBA'.
	self assert: (5827980550840017565077671610 storeStringBase: 21) equals: '21rKJIHGFEDCBA9876543210'.
	self assert: 5827980550840017565077671610 storeStringHex equals: '16r12D4CAE2B8A09BCFDBE30EBA'.
	self assert: (340653664490377789692799452102 printStringBase: 22) equals: 'LKJIHGFEDCBA9876543210'.
	self assert: (340653664490377789692799452102 radix: 22) equals: 'LKJIHGFEDCBA9876543210'.
	self assert: 340653664490377789692799452102 printStringHex equals: '44CB61B5B47E1A5D8F88583C6'.
	self assert: (340653664490377789692799452102 storeStringBase: 22) equals: '22rLKJIHGFEDCBA9876543210'.
	self assert: 340653664490377789692799452102 storeStringHex equals: '16r44CB61B5B47E1A5D8F88583C6'.
	self assert: (20837326537038308910317109288851 printStringBase: 23) equals: 'MLKJIHGFEDCBA9876543210'.
	self assert: (20837326537038308910317109288851 radix: 23) equals: 'MLKJIHGFEDCBA9876543210'.
	self assert: 20837326537038308910317109288851 printStringHex equals: '1070108876456E0EF115B389F93'.
	self assert: (20837326537038308910317109288851 storeStringBase: 23) equals: '23rMLKJIHGFEDCBA9876543210'.
	self assert: 20837326537038308910317109288851 storeStringHex equals: '16r1070108876456E0EF115B389F93'.
	self assert: (1331214537196502869015340298036888 printStringBase: 24) equals: 'NMLKJIHGFEDCBA9876543210'.
	self assert: (1331214537196502869015340298036888 radix: 24) equals: 'NMLKJIHGFEDCBA9876543210'.
	self assert: 1331214537196502869015340298036888 printStringHex equals: '41A24A285154B026B6ED206C6698'.
	self assert: (1331214537196502869015340298036888 storeStringBase: 24) equals: '24rNMLKJIHGFEDCBA9876543210'.
	self assert: 1331214537196502869015340298036888 storeStringHex equals: '16r41A24A285154B026B6ED206C6698'.
	self assert: (88663644327703473714387251271141900 printStringBase: 25) equals: 'ONMLKJIHGFEDCBA9876543210'.
	self assert: (88663644327703473714387251271141900 radix: 25) equals: 'ONMLKJIHGFEDCBA9876543210'.
	self assert: 88663644327703473714387251271141900 printStringHex equals: '111374860A2C6CEBE5999630398A0C'.
	self assert: (88663644327703473714387251271141900 storeStringBase: 25) equals: '25rONMLKJIHGFEDCBA9876543210'.
	self assert: 88663644327703473714387251271141900 storeStringHex equals: '16r111374860A2C6CEBE5999630398A0C'.
	self assert: (6146269788878825859099399609538763450 printStringBase: 26) equals: 'PONMLKJIHGFEDCBA9876543210'.
	self assert: (6146269788878825859099399609538763450 radix: 26) equals: 'PONMLKJIHGFEDCBA9876543210'.
	self assert: 6146269788878825859099399609538763450 printStringHex equals: '49FBA7F30B0F48BD14E6A99BD8ADABA'.
	self assert: (6146269788878825859099399609538763450 storeStringBase: 26) equals: '26rPONMLKJIHGFEDCBA9876543210'.
	self assert: 6146269788878825859099399609538763450 storeStringHex equals: '16r49FBA7F30B0F48BD14E6A99BD8ADABA'.
	self assert: (442770531899482980347734468443677777577 printStringBase: 27) equals: 'QPONMLKJIHGFEDCBA9876543210'.
	self assert: (442770531899482980347734468443677777577 radix: 27) equals: 'QPONMLKJIHGFEDCBA9876543210'.
	self assert: 442770531899482980347734468443677777577 printStringHex equals: '14D1A80A997343640C1145A073731DEA9'.
	self assert: (442770531899482980347734468443677777577 storeStringBase: 27) equals: '27rQPONMLKJIHGFEDCBA9876543210'.
	self assert: 442770531899482980347734468443677777577 storeStringHex equals: '16r14D1A80A997343640C1145A073731DEA9'.
	self assert: (33100056003358651440264672384704297711484 printStringBase: 28) equals: 'RQPONMLKJIHGFEDCBA9876543210'.
	self assert: (33100056003358651440264672384704297711484 radix: 28) equals: 'RQPONMLKJIHGFEDCBA9876543210'.
	self assert: 33100056003358651440264672384704297711484 printStringHex equals: '6145B6E6DACFA25D0E936F51D25932377C'.
	self assert: (33100056003358651440264672384704297711484 storeStringBase: 28) equals: '28rRQPONMLKJIHGFEDCBA9876543210'.
	self assert: 33100056003358651440264672384704297711484 storeStringHex equals: '16r6145B6E6DACFA25D0E936F51D25932377C'.
	self assert: (2564411043271974895869785066497940850811934 printStringBase: 29) equals: 'SRQPONMLKJIHGFEDCBA9876543210'.
	self assert: (2564411043271974895869785066497940850811934 radix: 29) equals: 'SRQPONMLKJIHGFEDCBA9876543210'.
	self assert: 2564411043271974895869785066497940850811934 printStringHex equals: '1D702071CBA4A1597D4DD37E95EFAC79241E'.
	self assert: (2564411043271974895869785066497940850811934 storeStringBase: 29) equals: '29rSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: 2564411043271974895869785066497940850811934 storeStringHex equals: '16r1D702071CBA4A1597D4DD37E95EFAC79241E'.
	self assert: (205646315052919334126040428061831153388822830 printStringBase: 30) equals: 'TSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: (205646315052919334126040428061831153388822830 radix: 30) equals: 'TSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: 205646315052919334126040428061831153388822830 printStringHex equals: '938B4343B54B550989989D02998718FFB212E'.
	self assert: (205646315052919334126040428061831153388822830 storeStringBase: 30) equals: '30rTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: 205646315052919334126040428061831153388822830 storeStringHex equals: '16r938B4343B54B550989989D02998718FFB212E'.
	self assert: (17050208381689099029767742314582582184093573615 printStringBase: 31) equals: 'UTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: (17050208381689099029767742314582582184093573615 radix: 31) equals: 'UTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: 17050208381689099029767742314582582184093573615 printStringHex equals: '2FC8ECB1521BA16D24A69E976D53873E2C661EF'.
	self assert: (17050208381689099029767742314582582184093573615 storeStringBase: 31) equals: '31rUTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: 17050208381689099029767742314582582184093573615 storeStringHex equals: '16r2FC8ECB1521BA16D24A69E976D53873E2C661EF'.
	self assert: (1459980823972598128486511383358617792788444579872 printStringBase: 32) equals: 'VUTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: (1459980823972598128486511383358617792788444579872 radix: 32) equals: 'VUTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: 1459980823972598128486511383358617792788444579872 printStringHex equals: 'FFBBCDEB38BDAB49CA307B9AC5A928398A418820'.
	self assert: (1459980823972598128486511383358617792788444579872 storeStringBase: 32) equals: '32rVUTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: 1459980823972598128486511383358617792788444579872 storeStringHex equals: '16rFFBBCDEB38BDAB49CA307B9AC5A928398A418820'.
	self assert: (128983956064237823710866404905431464703849549412368 printStringBase: 33) equals: 'WVUTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: (128983956064237823710866404905431464703849549412368 radix: 33) equals: 'WVUTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: 128983956064237823710866404905431464703849549412368 printStringHex equals: '584120A0328DE272AB055A8AA003CE4A559F223810'.
	self assert: (128983956064237823710866404905431464703849549412368 storeStringBase: 33) equals: '33rWVUTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: 128983956064237823710866404905431464703849549412368 storeStringHex equals: '16r584120A0328DE272AB055A8AA003CE4A559F223810'.
	self assert: (11745843093701610854378775891116314824081102660800418 printStringBase: 34) equals: 'XWVUTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: (11745843093701610854378775891116314824081102660800418 radix: 34) equals: 'XWVUTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: 11745843093701610854378775891116314824081102660800418 printStringHex equals: '1F64D4FC76000F7B92CF0CD5D0F350139AB9F25D8FA2'.
	self assert: (11745843093701610854378775891116314824081102660800418 storeStringBase: 34) equals: '34rXWVUTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: 11745843093701610854378775891116314824081102660800418 storeStringHex equals: '16r1F64D4FC76000F7B92CF0CD5D0F350139AB9F25D8FA2'.
	self assert: (1101553773143634726491620528194292510495517905608180485 printStringBase: 35) equals: 'YXWVUTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: (1101553773143634726491620528194292510495517905608180485 radix: 35) equals: 'YXWVUTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: 1101553773143634726491620528194292510495517905608180485 printStringHex equals: 'B8031AD55AD1FAA89E07A271CA1ED2F420415D1570305'.
	self assert: (1101553773143634726491620528194292510495517905608180485 storeStringBase: 35) equals: '35rYXWVUTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: 1101553773143634726491620528194292510495517905608180485 storeStringHex equals: '16rB8031AD55AD1FAA89E07A271CA1ED2F420415D1570305'.
	self assert: (106300512100105327644605138221229898724869759421181854980 printStringBase: 36) equals: 'ZYXWVUTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: (106300512100105327644605138221229898724869759421181854980 radix: 36) equals: 'ZYXWVUTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: 106300512100105327644605138221229898724869759421181854980 printStringHex equals: '455D441E55A37239AB4C303189576071AF5578FFCA80504'.
	self assert: (106300512100105327644605138221229898724869759421181854980 storeStringBase: 36) equals: '36rZYXWVUTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: 106300512100105327644605138221229898724869759421181854980 storeStringHex equals: '16r455D441E55A37239AB4C303189576071AF5578FFCA80504'.
]

{ #category : #'tests - printing' }
IntegerTest >> testPrintOnBaseShowRadix [
	self assert: (String streamContents: [ :str | 123 printOn: str base: 10 showRadix: false ]) equals: '123'.
	self assert: (String streamContents: [ :str | 123 printOn: str base: 10 showRadix: true ]) equals: '10r123'.
	self assert: (String streamContents: [ :str | 123 printOn: str base: 8 showRadix: false ]) equals: '173'.
	self assert: (String streamContents: [ :str | 123 printOn: str base: 8 showRadix: true ]) equals: '8r173'
]

{ #category : #'tests - printing' }
IntegerTest >> testPrintSeparatedByEverySignedOn [
	| printer |
	printer := [ :integer :printSigned | 
		String streamContents: [ :stream | 
			integer
				printSeparatedBy: $.
				every: 3
				signed: printSigned
				on: stream ] ].
	self assert: (printer value: 123456789 value: false) equals: '123.456.789'.
	self assert: (printer value: -123456789 value: false) equals: '-123.456.789'.
	self assert: (printer value: 123456789 value: true) equals: '+123.456.789'.
	self assert: (printer value: -123456789 value: true) equals: '-123.456.789'
]

{ #category : #'tests - printing' }
IntegerTest >> testPrintStringBase [
	2 to: 32 do: [ :b | 
		1 to: 1000 // b do: [ :n | 
			| bRaisedToN |
			bRaisedToN := b raisedTo: n.
			self assert: (bRaisedToN - 1 printStringBase: b) equals: (String new: n withAll: (Character digitValue: b - 1)).
			self assert: (bRaisedToN printStringBase: b) equals: '1' , (String new: n withAll: $0).

			self assert: (bRaisedToN negated + 1 printStringBase: b) equals: '-' , (String new: n withAll: (Character digitValue: b - 1)).
			self assert: (bRaisedToN negated printStringBase: b) equals: '-1' , (String new: n withAll: $0) ] ]
]

{ #category : #'tests - printing' }
IntegerTest >> testPrintStringHex [

	self assert: 0 printStringHex equals: '0'.
	self assert: 12 printStringHex equals: 'C'.
	self assert: 1234 printStringHex equals: '4D2'
]

{ #category : #'tests - mathematical functions' }
IntegerTest >> testRaisedToErrorConditions [
	"
	IntegerTest new testRaisedToErrorConditions
	"

	self should: [ -2 raisedTo: 1/4 ] raise: ArithmeticError.
	self should: [ -2 raisedTo: 1.24 ] raise: ArithmeticError.
]

{ #category : #'tests - instance creation' }
IntegerTest >> testReadFrom [
	"Ensure remaining characters in a stream are not lost when parsing an integer."

	| rs i s |
	rs := '123s could be confused with a ScaledDecimal' readStream.
	i := Integer readFrom: rs.
	self assert: i equals: 123.
	s := rs upToEnd.
	self assert: 's could be confused with a ScaledDecimal' equals: s.
	rs := '123.s could be confused with a ScaledDecimal' readStream.
	i := Integer readFrom: rs.
	self assert: i equals: 123.
	s := rs upToEnd.
	self assert: '.s could be confused with a ScaledDecimal' equals: s
]

{ #category : #'tests - instance creation' }
IntegerTest >> testReadFromWithError [
	"Ensure that a string that does not represent an integer raises an error."
	
	self should: [Integer readFrom: 'invalid'] raise: Error

]

{ #category : #'tests - arithmetic' }
IntegerTest >> testReciprocalModulo [
	1 to: 512 do: [ :a | 
		a + 1 to: 512 do: [ :b | 
			| c |
			(a gcd: b) = 1
				ifTrue: [ c := a reciprocalModulo: b.
					self assert: a * c \\ b equals: 1 ]
				ifFalse: [ self should: [ a reciprocalModulo: b ] raise: Error ] ] ]
]

{ #category : #'tests - rounding' }
IntegerTest >> testRounding [
 
	self assert: (5 round: 2) equals: 5
]

{ #category : #'tests - printing' }
IntegerTest >> testStoreOn [
	| integer |
	integer := 42.
	
	self assert: (String streamContents: [ :s | integer storeOn: s ]) equals: '42'.
	
	integer := -42.
	
	self assert: (String streamContents: [ :s | integer storeOn: s ]) equals: ' -42'.
]

{ #category : #'tests - instance creation' }
IntegerTest >> testStringAsNumber [
	"This covers parsing in Number>>readFrom:
	Trailing decimal points should be ignored."

	self assert: '123' asNumber equals: 123.
	self assert: '-123' asNumber equals: -123.
	self assert: '123.' asNumber equals: 123.
	self assert: '-123.' asNumber equals: -123.
	self assert: '123This is not to be read' asNumber equals: 123.
	self assert: '123s could be confused with a ScaledDecimal' asNumber equals: 123.
	self assert: '123e could be confused with a Float' asNumber equals: 123
]

{ #category : #'tests - bitLogic' }
IntegerTest >> testTwoComplementBitLogicWithCarry [
	"This is non regression test for http://bugs.squeak.org/view.php?id=6874"
	
	"By property of two complement, following operation is:
	...111110000 this is -16
	...111101111 this is -16-1
	...111100000 this is -32, the result of bitAnd: on two complement
	
	This test used to fail with n=31 39 47.... because of bug 6874"
	
	self assert: ((2 to: 80) allSatisfy: [:n | ((2 raisedTo: n) negated bitAnd: (2 raisedTo: n) negated - 1) = (2 raisedTo: n + 1) negated]).
]

{ #category : #'tests - bitLogic' }
IntegerTest >> testTwoComplementRightShift [
	| large small |
	small := 2 << 16.
	large := 2 << 32.
	self assert: (small negated bitShift: -1) ~= ((small + 1) negated bitShift: -1) identicalTo: (large negated bitShift: -1) ~= ((large + 1) negated bitShift: -1).

	self assert: (small bitShift: -1) ~= (small + 1 bitShift: -1) identicalTo: (large bitShift: -1) ~= (large + 1 bitShift: -1)
]
