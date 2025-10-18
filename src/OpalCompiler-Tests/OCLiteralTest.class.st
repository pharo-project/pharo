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
OCLiteralTest >> testFloatAndIntegerDoNotShareSameEntryEvenIfEqual [

	self
		assertObject: 1
		isNotLiteralIdenticalTo: 1 asFloat
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
