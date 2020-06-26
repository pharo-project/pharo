"
I provide a test suite for SmallInteger values. Examine my tests to see how SmallIntegers should behave, and see how to use them.
"
Class {
	#name : #SmallIntegerTest,
	#superclass : #ClassTestCase,
	#category : #'Kernel-Tests-Numbers'
}

{ #category : #'tests - Class Methods' }
SmallIntegerTest >> testBasicNew [

	self should: [SmallInteger basicNew] raise: self defaultTestError. 
]

{ #category : #'tests - operations' }
SmallIntegerTest >> testBitAnd [
	| onezero zeroone ones |
	onezero := 2r1010101.
	zeroone := 2r0101010.
	ones    := 2r1111111.
	 
	self assert: (onezero bitAnd: ones)    equals: onezero.
	self assert: (onezero bitAnd: zeroone) equals: 0.
	self assert: (onezero bitAnd: onezero) equals: onezero.
	self assert: (onezero bitAnd: 0)       equals: 0.
]

{ #category : #'tests - operations' }
SmallIntegerTest >> testBitOr [
	| onezero zeroone ones |
	onezero := 2r1010101.
	zeroone := 2r0101010.
	ones    := 2r1111111.
	
	self assert: (onezero bitOr: ones)    equals: ones.
	self assert: (onezero bitOr: zeroone) equals: ones.
	self assert: (onezero bitOr: onezero) equals: onezero.
	self assert: (onezero bitOr: 0)       equals: onezero.
]

{ #category : #'tests - operations' }
SmallIntegerTest >> testBitShift [
	 
	self assert: (2r11 bitShift: 1) equals: 2r110.
	self assert: (2r11 bitShift: -1) equals: 2r1.
	self assert: (2r1011 bitShift: 2) equals: 2r101100.
	self assert: (2r1011 bitShift: -2) equals: 2r10.
	self assert: (2r11 bitShift: -2) equals: 0
]

{ #category : #'tests - operations' }
SmallIntegerTest >> testBitXOr [
	| onezero zeroone ones |
	onezero := 2r1010101.
	zeroone := 2r0101010.
	ones    := 2r1111111.
	self assert: (onezero bitXor: ones)    equals: zeroone.
	self assert: (onezero bitXor: zeroone) equals: ones.
	self assert: (onezero bitXor: onezero) equals: 0.
	self assert: (onezero bitXor: 0)       equals: onezero.
]

{ #category : #'tests - Class Methods' }
SmallIntegerTest >> testByteAt [

	| number |
	number := 16rABCDEF.
	self assert: (number byteAt: 1) equals: 16rEF.
	self assert: (number byteAt: 2) equals: 16rCD.
	self assert: (number byteAt: 3) equals: 16rAB.
]

{ #category : #'tests - operations' }
SmallIntegerTest >> testCeiling [
	self assert: 2 ceiling equals: 2.
	self assert: -2 ceiling equals: -2.
	self assert: 2.1 ceiling equals: 3.
	self assert: -2.1 ceiling equals: -2
]

{ #category : #'tests - arithmetic' }
SmallIntegerTest >> testDivide [
	self assert: 2 / 1 equals: 2.
	self assert: (3 / 2) isFraction.
	self assert: 4 / 2 equals: 2.
	self should: [ 1 / 0 ] raise: ZeroDivide
]

{ #category : #'tests - operations' }
SmallIntegerTest >> testGcd [
	
	"trick case: a = b"
 	self assert: (13 gcd: 13) equals: 13.
	"receiver is a prime"
	self assert: (37 gcd: 600) equals: 1.
	"one is multiplum of other"
	self assert: (20 gcd: 100) equals: 20. 
	"straight case"
   self assert: (624129 gcd: 2061517) equals: 18913
]

{ #category : #'tests - other' }
SmallIntegerTest >> testIsNotAbstract [

	self deny: SmallInteger isAbstract 
]

{ #category : #'tests - Class Methods' }
SmallIntegerTest >> testMaxVal [
	Smalltalk vm wordSize = 4 ifTrue: [ self assert: SmallInteger maxVal equals: 16r3FFFFFFF ].
	Smalltalk vm wordSize = 8 ifTrue: [ self assert: SmallInteger maxVal equals: 16rFFFFFFFFFFFFFFF ]
]

{ #category : #'tests - Class Methods' }
SmallIntegerTest >> testMinVal [
	Smalltalk vm wordSize = 4 ifTrue: [ self assert: SmallInteger minVal equals: -16r40000000 ].
	Smalltalk vm wordSize = 8 ifTrue: [ self assert: SmallInteger minVal equals: -16r1000000000000000 ]
]

{ #category : #'tests - Class Methods' }
SmallIntegerTest >> testNew [

	self should: [SmallInteger new] raise: self defaultTestError. 
]

{ #category : #'tests - printing' }
SmallIntegerTest >> testPrintPaddedWith [
	self assert: (123 printPaddedWith: $0 to: 10 base: 2) equals: '0001111011'.
	self assert: (123 printPaddedWith: $0 to: 10 base: 8) equals: '0000000173'.
	self assert: (123 printPaddedWith: $0 to: 10 base: 10) equals: '0000000123'.
	self assert: (123 printPaddedWith: $0 to: 10 base: 16) equals: '000000007B'
]

{ #category : #'tests - printing' }
SmallIntegerTest >> testPrintString [
	self assert: 1 printString equals: '1'.
	self assert: -1 printString equals: '-1'.
	Smalltalk vm wordSize = 4
		ifTrue: [ 
			self assert: SmallInteger minVal printString equals: '-1073741824'.
			self assert: SmallInteger maxVal printString equals: '1073741823' ].
	Smalltalk vm wordSize = 8
		ifTrue: [ 
			self assert: SmallInteger minVal printString equals: '-1152921504606846976'.
			self assert: SmallInteger maxVal printString equals: '1152921504606846975' ].
	self assert: 12345 printString equals: '12345'.
	self assert: -54321 printString equals: '-54321'.
	
	self assert: 0 decimalDigitLength equals: 1.
	self assert: 4 decimalDigitLength equals: 1.
	self assert: 12 decimalDigitLength equals: 2.
	self assert: 123 decimalDigitLength equals: 3.
	self assert: 1234 decimalDigitLength equals: 4.
	self assert: 56789 decimalDigitLength equals: 5.
	self assert: 657483 decimalDigitLength equals: 6.
	self assert: 6571483 decimalDigitLength equals: 7.
	self assert: 65174383 decimalDigitLength equals: 8.
	self assert: 625744831 decimalDigitLength equals: 9.
	self assert: 1000001111 decimalDigitLength equals: 10.
	Smalltalk vm wordSize = 4
		ifTrue: [ self assert: SmallInteger maxVal decimalDigitLength equals: 10 ].
	Smalltalk vm wordSize = 8
		ifTrue: [ self assert: SmallInteger maxVal decimalDigitLength equals: 19 ].
]

{ #category : #tests }
SmallIntegerTest >> testQuo [
	self should: [ 1 quo: 0] raise: ZeroDivide.
	self assert: ( 1 quo: 2) equals:  0.
	self assert: ( 4 quo: 2) equals:  2.
	self assert: (-4 quo: 2) equals: -2.
	self assert: (-5 quo: 2) equals: -2.
	self assert: ( 5 quo: 2) equals:  2
]
