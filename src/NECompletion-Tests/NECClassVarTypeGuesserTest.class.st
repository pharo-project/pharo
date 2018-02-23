Class {
	#name : #NECClassVarTypeGuesserTest,
	#superclass : #TestCase,
	#category : #'NECompletion-Tests'
}

{ #category : #utilities }
NECClassVarTypeGuesserTest >> guessVariable: aString in: aClass expected: theClassExpected [ 
	| typeGuesser result |
	typeGuesser := NECClassVarTypeGuesser 
		variableName: aString
		class: aClass.
	result := typeGuesser perform.
	self assert: result == theClassExpected
]

{ #category : #tests }
NECClassVarTypeGuesserTest >> testClassVar [
	self
		guessVariable: 'ClassVar'
		in: NECTestClass
		expected: ByteString
]

{ #category : #tests }
NECClassVarTypeGuesserTest >> testDependentsFieldsClassVar [
	self
		guessVariable: 'DependentsFields'
		in: NECTestClass
		expected: WeakIdentityKeyDictionary
]

{ #category : #tests }
NECClassVarTypeGuesserTest >> testSuperClassVar [
	self
		guessVariable: 'SuperClassVar'
		in: NECTestClass
		expected: Dictionary 
]
