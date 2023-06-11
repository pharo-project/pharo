Class {
	#name : #RBData2Test,
	#superclass : #TestCase,
	#instVars : [
		'aString'
	],
	#category : #'Refactoring-DataForTesting'
}

{ #category : #running }
RBData2Test >> accessToAClass [
	RBRefactoryTestDataApp new
]

{ #category : #tests }
RBData2Test >> someMethod [
	#'some.initializations'
]

{ #category : #running }
RBData2Test >> testExtractSetupExample1 [
	self accessToAClass.
	aString := 'Example'.
	self assert: 4 > 5 equals: false
]

{ #category : #running }
RBData2Test >> testExtractSetupExample2 [
	"Example"
	self accessToAClass.
	aString := 'Example'.
	self assert: true
]

{ #category : #running }
RBData2Test >> testExtractSetupExample3 [
	"Example"
	self accessToAClass.
	"Comment"
	aString := 'Example'.
	self deny: false
]

{ #category : #running }
RBData2Test >> testExtractSetupExample4 [
	self assert: true.
	self deny: false
]

{ #category : #tests }
RBData2Test >> testExtractSetupExample5 [
	self someMethod.
	self assert: true
]

{ #category : #tests }
RBData2Test >> testExtractSetupExample6 [
	| aString aNumber |
	aString := 'Some string'.
	self someMethod.
	aNumber := 4.
	self assert: aString isNotEmpty.
	self deny: (aString, aNumber asString) isEmpty.
	self assert: true
]

{ #category : #tests }
RBData2Test >> testExtractSetupExample7 [
	| aString aNumber |
	aString := 'sa'.
	self someMethod.
	aNumber := 4.
	self assert: aString isNotEmpty
]
