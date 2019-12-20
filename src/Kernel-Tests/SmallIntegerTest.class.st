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
