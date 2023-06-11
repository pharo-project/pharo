Class {
	#name : #RBDataTest,
	#superclass : #TestCase,
	#category : #'Refactoring-DataForTesting'
}

{ #category : #tests }
RBDataTest >> testExtractSetupExample0 [
	self someMethod.
	self assert: true
]

{ #category : #tests }
RBDataTest >> testExtractSetupExample1 [
	| aString aNumber |
	aString := 'Some string'.
	self someMethod.
	aNumber := 4.
	self assert: aString isNotEmpty.
	self deny: (aString, aNumber asString) isEmpty.
	self assert: true
]

{ #category : #tests }
RBDataTest >> testExtractSetupExample2 [
	| aString aNumber |
	aString := 'sa'.
	self someMethod.
	aNumber := 4.
	self assert: aString isNotEmpty
]
