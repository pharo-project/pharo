Class {
	#name : #NECInstVarTypeGuesserTest,
	#superclass : #TestCase,
	#category : #'NECompletion-Tests'
}

{ #category : #private }
NECInstVarTypeGuesserTest >> guessVariable: aString in: aClass expected: theClassExpected [ 
	| typeGuesser result |
	typeGuesser := NECInstVarTypeGuesser 
		variableName: aString
		class: aClass.
	result := typeGuesser perform.
	self assert: result == theClassExpected
]

{ #category : #testing }
NECInstVarTypeGuesserTest >> testComplexInit [
	self
		guessVariable: 'complexInit'
		in: NECTestClass
		expected: nil
]

{ #category : #testing }
NECInstVarTypeGuesserTest >> testComplexInit2 [
	self 
		guessVariable: 'complexInit2'
		in: NECTestClass
		expected: Dictionary
]

{ #category : #testing }
NECInstVarTypeGuesserTest >> testConstantArray [
	self 
		guessVariable: 'constantArray'
		in: NECTestClass
		expected: Array
]

{ #category : #testing }
NECInstVarTypeGuesserTest >> testConstantBoolean [
	self 
		guessVariable: 'constantBoolean'
		in: NECTestClass
		expected: True
]

{ #category : #testing }
NECInstVarTypeGuesserTest >> testConstantInteger [
	self 
		guessVariable: 'constantInteger'
		in: NECTestClass
		expected: SmallInteger
]

{ #category : #testing }
NECInstVarTypeGuesserTest >> testConstantIntegerNil [
	self
		guessVariable: 'constantNil'
		in: NECTestClass
		expected: nil
]

{ #category : #testing }
NECInstVarTypeGuesserTest >> testConstantString [
	self
		guessVariable: 'constantString'
		in: NECTestClass
		expected: ByteString
]

{ #category : #testing }
NECInstVarTypeGuesserTest >> testConstantSymbol [
	self
		guessVariable: 'constantSymbol'
		in: NECTestClass
		expected: ByteSymbol
]

{ #category : #testing }
NECInstVarTypeGuesserTest >> testGlobalVarKeyword [
	self
		guessVariable: 'globalVarKeyword'
		in: NECTestClass
		expected: SortedCollection
]

{ #category : #testing }
NECInstVarTypeGuesserTest >> testGlobalVarKeyword2 [
	self
		guessVariable: 'globalVarKeyword2'
		in: NECTestClass
		expected: SortedCollection
]

{ #category : #testing }
NECInstVarTypeGuesserTest >> testMessageSend [
	self 
		guessVariable: 'messageSend'
		in: NECTestClass
		expected: Dictionary
]

{ #category : #testing }
NECInstVarTypeGuesserTest >> testMessageSend2 [
	self 
		guessVariable: 'messageSend2'
		in: NECTestClass
		expected: nil
]

{ #category : #testing }
NECInstVarTypeGuesserTest >> testSuperWithAnotherInit [
	self 
		guessVariable: 'superInstVar'
		in: NECTestClass
		expected: Dictionary
]

{ #category : #testing }
NECInstVarTypeGuesserTest >> testTypeSuggestingParameter [
	self 
		guessVariable: 'typeSuggestingParameter2'
		in: NECTestClass
		expected: Rectangle
]
