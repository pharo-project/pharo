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

{ #category : #tests }
RBExtractSetUpMethodParametrizedTest >> testExtractSetUp [
	| class refactoring |
	refactoring := self createRefactoringWithArguments: { (14+12 to: 29+12) . #testExtractSetupExample5 . RBData2Test selectors}.
	self executeRefactoring: refactoring.
	class := refactoring model classNamed: #RBData2Test.

	"Pay attention that method body is created this way to support potential selector rename"
	self 
		assert: (class parseTreeForSelector: #testExtractSetupExample5) 
		equals: (self parseMethod: #testExtractSetupExample5 asString, Character cr asString,
'	self assert: true').

	self 
		assert: (class parseTreeForSelector: #setUp) 
		equals: (self parseMethod: 'setUp
	super setUp.
	self someMethod')
]

{ #category : #tests }
RBExtractSetUpMethodParametrizedTest >> testExtractSetUpAndConvertTempsToInstVars [
	| class refactoring |
	refactoring := self createRefactoringWithArguments: { (36+12  to: 77+12) . #testExtractSetupExample1 . RBData2Test }.
	self executeRefactoring: refactoring.
	class := refactoring model classNamed: #RBData2Test.
	self assert: (class instanceVariableNames includes: #aString).
	
	"Pay attention that method body is created this way to support potential selector rename"
	self 
		assert: (class parseTreeForSelector: #testExtractSetupExample1) 
		equals: (self parseMethod: #testExtractSetupExample1 asString, Character cr asString,
'	| aNumber |
	aNumber := 4.
	self assert: aString isNotEmpty.
	self deny: (aString , aNumber asString) isEmpty.
	self assert: true').
	
	self 
		assert: (class parseTreeForSelector: #testExtractSetupExample2) 
		equals: (self parseMethod: #testExtractSetupExample2 asString, Character cr asString,
'	| aNumber |
	aString := ''sa''.
	self someMethod.
	aNumber := 4.
	self assert: aString isNotEmpty.').
	
	self 
		assert: (class parseTreeForSelector: #setUp) 
		equals: (self parseMethod: 'setUp
	super setUp.
	aString := ''Some string''.
	self someMethod.')
]

{ #category : #tests }
RBExtractSetUpMethodParametrizedTest >> testExtractSetUpAndConvertTwoTempsToInstVars [
	| class refactoring |
	refactoring := self createRefactoringWithArguments: { (36+12 to: 93+12) . #testExtractSetupExample1 . RBData2Test }.
	self executeRefactoring: refactoring.
	class := refactoring model classNamed: #RBData2Test.
	self assert: (class instanceVariableNames includes: #aString).
	self assert: (class instanceVariableNames includes: #aNumber).
	
	"Pay attention that method body is created this way to support potential selector rename"
	
	self assert: (class parseTreeForSelector: #testExtractSetupExample1) equals: (self parseMethod: #testExtractSetupExample1 asString, Character cr asString,
'self assert: aString isNotEmpty.
	self deny: (aString , aNumber asString) isEmpty.
	self assert: true').
	
	self assert: (class parseTreeForSelector: #testExtractSetupExample2) equals: (self parseMethod: #testExtractSetupExample2 asString, Character cr asString,
'	aString := ''sa''.
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
RBExtractSetUpMethodParametrizedTest >> testFailureBadClass [

	self shouldFail: (self createRefactoringWithArguments: {
				 (78 to: 197).
				 #displayName.
				 RBLintRuleTestData })
]

{ #category : #'failure tests' }
RBExtractSetUpMethodParametrizedTest >> testFailureBadInterval [
	self shouldFail: (self createRefactoringWithArguments:
		{ (14 to: 35) . #testExtractSetupExample1 . RBData2Test })
]

{ #category : #'failure tests' }
RBExtractSetUpMethodParametrizedTest >> testFailureExtractSetUpWhenIsNotFirstsSentences [

	self shouldFail: (self createRefactoringWithArguments: {
				 (63 to: 77).
				 #testExtractSetupExample1.
				 RBData2Test })
]

{ #category : #'failure tests' }
RBExtractSetUpMethodParametrizedTest >> testFailureModelExistingSetUpMethod [

	| class |
	model := RBClassModelFactory rbNamespace new.
	class := model classNamed: #RBData2Test.
	class compile: 'setUp #setUp' classified: #( #accessing ).
	self shouldFail:
		(self createRefactoringWithModel: model andArguments: {
				 (14 to: 29).
				 #testExtractSetupExample5.
				 RBData2Test })
]
