Class {
	#name : 'OCLiteralTest',
	#superclass : 'TestCase',
	#category : 'OpalCompiler-Tests-Misc',
	#package : 'OpalCompiler-Tests',
	#tag : 'Misc'
}

{ #category : 'tests' }
OCLiteralTest >> assertObject: anObject isLiteralIdenticalTo: anotherObject [

	| literals |
	literals := OCLiteralList new.

	self
		assert: (literals addLiteral: anObject)
		equals: (literals addLiteral: anotherObject)
]

{ #category : 'tests' }
OCLiteralTest >> assertObject: anObject isNotLiteralIdenticalTo: anotherObject [

	| literals |
	literals := OCLiteralList new.

	self assert: anObject equals: anotherObject.
	self
		deny: (literals addLiteral: anObject)
		equals: (literals addLiteral: anotherObject)
]

{ #category : 'tests' }
OCLiteralTest >> testArrayIsLiteral [

	| aLiteralArray |
	aLiteralArray := #( 1 true 3 #four ) copy.
	self assert: aLiteralArray isLiteral.
	aLiteralArray at: 1 put: self class.
	self deny: aLiteralArray isLiteral
]

{ #category : 'tests' }
OCLiteralTest >> testByteArrayIsLiteral [

	self
		assert: #[ 122 43 213 7 ] isLiteral
		description: 'ByteArray instances are literal'.
	self
		deny: thisContext method isLiteral
		description: 'ByteArray sub instances are not literal'
]

{ #category : 'tests' }
OCLiteralTest >> testCyclicArrayIsLiteral [

	| aLiteralArray |
	aLiteralArray := #( 1 true 3 #four ) copy.

	aLiteralArray at: 1 put: aLiteralArray.
	self deny: aLiteralArray isLiteral
]

{ #category : 'tests' }
OCLiteralTest >> testFloatAndIntegerDoNotShareSameEntryEvenIfEqual [

	self
		assertObject: 1
		isNotLiteralIdenticalTo: 1 asFloat
]

{ #category : 'tests' }
OCLiteralTest >> testFloatNanIsNotLiteral [

	self
		deny: Float nan isLiteral
		description: 'there is no literal representation of NaN'
]

{ #category : 'tests' }
OCLiteralTest >> testNilIsLiteral [

	self assert: nil isLiteral
]

{ #category : 'tests' }
OCLiteralTest >> testRandomObjectsOfTheSameClassDoNotShareSameEntryEvenIfEqual [

	"Use a copy to avoid any kind of compiler optimization"
	self
		assertObject: (1@2) copy
		isNotLiteralIdenticalTo: (1@2) copy
]

{ #category : 'tests' }
OCLiteralTest >> testScaleDecimalsWithDifferentScalesDoNotShareSameEntryEvenIfEqual [

	self
		assertObject: 1.01s2
		isNotLiteralIdenticalTo: 1.01s5
]

{ #category : 'tests' }
OCLiteralTest >> testScaledDecimalIsLiteral [

	self
		assert: 1.00s2 isLiteral
		description: 'every literal obviously isLiteral'.

	"Note that (1 / 3.00s2) is not a well behaved literal,
	because it does not re-evaluate to self...
	Every literal should be evaluated as self (see isSelfEvaluating).
	There is currently no way to print it as a literal.
	So i propose it shall not answer true."
	self
		deny: (1 / 3.00s2) isLiteral
		description: 'this number cannot represent itself as a literal'
]

{ #category : 'tests' }
OCLiteralTest >> testTwoEqualFloatsShareSameLiteralEntry [

	self
		assertObject: 1 asFloat
		isLiteralIdenticalTo: 1 asFloat
]

{ #category : 'tests' }
OCLiteralTest >> testTwoEqualIntegersShareSameLiteralEntry [

	self
		assertObject: 1
		isLiteralIdenticalTo: 1
]

{ #category : 'tests' }
OCLiteralTest >> testTwoEqualStringsShareSameLiteralEntry [

	"Use a string and force copy it to guarantee we have two different strings"
	| theString |
	theString := 'someString'.
	self
		assertObject: theString copy
		isLiteralIdenticalTo: theString copy
]

{ #category : 'tests' }
OCLiteralTest >> testTwoEqualSymbolsShareSameLiteralEntry [

	self
		assertObject: (Symbol intern: 'symbol')
		isLiteralIdenticalTo: (Symbol intern: 'symbol')
]

{ #category : 'tests' }
OCLiteralTest >> testWeakArrayIsNotLiteral [
	"We work with a copy of literalArray, to avoid corrupting the code."

	self
		deny: (WeakArray withAll: #( 1 true 3 #four )) isLiteral
		description: 'instances of Array subclasses are not literal'
]
