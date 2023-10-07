Class {
	#name : 'RBTestAsDataForExtractSetupTransformationTest',
	#superclass : 'TestCase',
	#instVars : [
		'aString'
	],
	#category : 'Refactoring-DataForTesting-ForTestRelatedOperation',
	#package : 'Refactoring-DataForTesting',
	#tag : 'ForTestRelatedOperation'
}

{ #category : 'running' }
RBTestAsDataForExtractSetupTransformationTest >> accessToAClass [
	RBClassDataForRefactoringTest new
]

{ #category : 'tests' }
RBTestAsDataForExtractSetupTransformationTest >> someMethod [
	#'some.initializations'
]

{ #category : 'running' }
RBTestAsDataForExtractSetupTransformationTest >> testExtractSetupExample1 [
	self accessToAClass.
	aString := 'Example'.
	self assert: 4 > 5 equals: false
]

{ #category : 'running' }
RBTestAsDataForExtractSetupTransformationTest >> testExtractSetupExample2 [
	"Example"
	self accessToAClass.
	aString := 'Example'.
	self assert: true
]

{ #category : 'running' }
RBTestAsDataForExtractSetupTransformationTest >> testExtractSetupExample3 [
	"Example"
	self accessToAClass.
	"Comment"
	aString := 'Example'.
	self deny: false
]

{ #category : 'running' }
RBTestAsDataForExtractSetupTransformationTest >> testExtractSetupExample4 [
	self assert: true.
	self deny: false
]

{ #category : 'tests' }
RBTestAsDataForExtractSetupTransformationTest >> testExtractSetupExample5 [
	self someMethod.
	self assert: true
]

{ #category : 'tests' }
RBTestAsDataForExtractSetupTransformationTest >> testExtractSetupExample6 [
	| aString2 aNumber |
	aString2 := 'Some string'.
	self someMethod.
	aNumber := 4.
	self assert: aString2 isNotEmpty.
	self deny: (aString2, aNumber asString) isEmpty.
	self assert: true
]

{ #category : 'tests' }
RBTestAsDataForExtractSetupTransformationTest >> testExtractSetupExample7 [
	| aString2 aNumber |
	aString2 := 'sa'.
	self someMethod.
	aNumber := 4.
	self assert: aString2 isNotEmpty
]

{ #category : 'tests' }
RBTestAsDataForExtractSetupTransformationTest >> testExtractSetupExample8 [
	| aString2 aNumber |
	aString2 := 'Some string'.
	self someMethod.
	aNumber := 4.
	"this is an example to see if the extracted as setup expressions are also extracted from this one too."
]
