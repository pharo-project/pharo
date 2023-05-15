Class {
	#name : #RBExtractSetUpMethodParametrizedTest,
	#superclass : #RBAbstractRefactoringTest,
	#category : #'Refactoring2-Transformations-Tests-SingleParametrized'
}

{ #category : #tests }
RBExtractSetUpMethodParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBExtractSetUpMethodRefactoring };
		yourself
]

{ #category : #accessing }
RBExtractSetUpMethodParametrizedTest >> constructor [
	^ #extract:from:in:
]

{ #category : #'failure tests' }
RBExtractSetUpMethodParametrizedTest >> testBadClass [
	self shouldFail: (self createRefactoringWithArguments:
		{ (78 to: 197) . #displayName . RBLintRuleTestData })
]

{ #category : #'failure tests' }
RBExtractSetUpMethodParametrizedTest >> testBadInterval [
	self shouldFail: (self createRefactoringWithArguments:
		{ (14 to: 35) . #testExample . RBDataTest })
]

{ #category : #tests }
RBExtractSetUpMethodParametrizedTest >> testExtractSetUp [
	| class refactoring |
	refactoring := self createRefactoringWithArguments: { (14 to: 29) . #testExample . RBDataTest }.
	self executeRefactoring: refactoring.
	class := refactoring model classNamed: #RBDataTest.

	self assert: (class parseTreeForSelector: #testExample) equals: (self parseMethod: 'testExample
	self assert: true').
	self assert: (class parseTreeForSelector: #setUp) equals: (self parseMethod: 'setUp
	super setUp.
	self someMethod')
]

{ #category : #tests }
RBExtractSetUpMethodParametrizedTest >> testExtractSetUpAndConvertTempsToInstVars [
	| class refactoring |
	refactoring := self createRefactoringWithArguments: { (36 to: 77) . #testExample1 . RBDataTest }.
	self executeRefactoring: refactoring.
	class := refactoring model classNamed: #RBDataTest.
	self assert: (class instanceVariableNames includes: #aString).
	self assert: (class parseTreeForSelector: #testExample1) equals: (self parseMethod: 'testExample1
	| aNumber |
	aNumber := 4.
	self assert: aString isNotEmpty.
	self deny: (aString , aNumber asString) isEmpty.
	self assert: true').
	self assert: (class parseTreeForSelector: #testExample2) equals: (self parseMethod: 'testExample2
	| aNumber |
	aString := ''sa''.
	self someMethod.
	aNumber := 4.
	self assert: aString isNotEmpty.').
	self assert: (class parseTreeForSelector: #setUp) equals: (self parseMethod: 'setUp
	super setUp.
	aString := ''Some string''.
	self someMethod.')
]

{ #category : #tests }
RBExtractSetUpMethodParametrizedTest >> testExtractSetUpAndConvertTwoTempsToInstVars [
	| class refactoring |
	refactoring := self createRefactoringWithArguments: { (36 to: 93) . #testExample1 . RBDataTest }.
	self executeRefactoring: refactoring.
	class := refactoring model classNamed: #RBDataTest.
	self assert: (class instanceVariableNames includes: #aString).
	self assert: (class instanceVariableNames includes: #aNumber).
	self assert: (class parseTreeForSelector: #testExample1) equals: (self parseMethod: 'testExample1
	self assert: aString isNotEmpty.
	self deny: (aString , aNumber asString) isEmpty.
	self assert: true').
	self assert: (class parseTreeForSelector: #testExample2) equals: (self parseMethod: 'testExample2
	aString := ''sa''.
	self someMethod.
	aNumber := 4.
	self assert: aString isNotEmpty.').
	self assert: (class parseTreeForSelector: #setUp) equals: (self parseMethod: 'setUp
	super setUp.
	aString := ''Some string''.
	self someMethod.
	aNumber := 4')
]

{ #category : #'failure tests' }
RBExtractSetUpMethodParametrizedTest >> testExtractSetUpWhenIsNotFirstsSentences [

	self shouldFail: (self createRefactoringWithArguments:
		{ (63 to: 77) . #testExample1 . RBDataTest })
]

{ #category : #'failure tests' }
RBExtractSetUpMethodParametrizedTest >> testModelExistingSetUpMethod [
	| class |
	model := RBClassModelFactory rbNamespace new.
	class := model classNamed: #RBDataTest.
	class compile: 'setUp #setUp'
		classified: #(#accessing).
	self shouldFail: ( self createRefactoringWithModel: model
		andArguments: { (14 to: 29) . #testExample . RBDataTest })
]
