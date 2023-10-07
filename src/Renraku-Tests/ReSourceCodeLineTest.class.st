"
An ALSourceCodeLineTest is a test class for testing the behavior of ALSourceCodeLine
"
Class {
	#name : 'ReSourceCodeLineTest',
	#superclass : 'TestCase',
	#category : 'Renraku-Tests-Utilities',
	#package : 'Renraku-Tests',
	#tag : 'Utilities'
}

{ #category : 'tests - instance creation' }
ReSourceCodeLineTest >> testInstanceCreation [

	| line |
	line := ReSourceCodeLine lineNumber: 1 contents: 'something'.
	self
		assert: line lineNumber equals: 1;
		assert: line contents equals: 'something'
]

{ #category : 'tests - instance creation' }
ReSourceCodeLineTest >> testOnlyContainsSeparators [
	| line |
	line := ReSourceCodeLine lineNumber: 1 contents: (String withAll: { Character space. Character tab }).
	self assert: line onlyContainsSeparators
]

{ #category : 'tests - instance creation' }
ReSourceCodeLineTest >> testRemoveTrailingDot [
	| line |
	line := ReSourceCodeLine lineNumber: 1 contents: 'hello.'.

	line removeTrailingDot.

	self assert: line contents equals: 'hello'
]

{ #category : 'tests - instance creation' }
ReSourceCodeLineTest >> testRemoveTrailingSeparators [
	| line |
	line := ReSourceCodeLine lineNumber: 1 contents: 'hello '.

	line removeTrailingSeparators.

	self assert: line contents equals: 'hello'
]
