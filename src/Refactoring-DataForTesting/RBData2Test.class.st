Class {
	#name : #RBData2Test,
	#superclass : #TestCase,
	#instVars : [
		'aString'
	],
	#category : #'Refactoring-DataForTesting-Refactorings'
}

{ #category : #running }
RBData2Test >> accessToAClass [
	RBRefactoryTestDataApp new
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
