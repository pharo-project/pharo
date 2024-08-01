Class {
	#name : 'RBExtractSetUpMethodParametrizedTest',
	#superclass : 'RBAbstractRefactoringTest',
	#category : 'Refactoring-Transformations-Tests-SingleParametrized',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'SingleParametrized'
}

{ #category : 'tests' }
RBExtractSetUpMethodParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBExtractSetUpMethodRefactoring };
		yourself
]

{ #category : 'accessing' }
RBExtractSetUpMethodParametrizedTest >> constructor [
	^ #extract:from:in:
]

{ #category : 'tests' }
RBExtractSetUpMethodParametrizedTest >> testExtractSetUp [
	| class refactoring |
	model := self modelOnClasses: { TestCase . RBTestAsDataForExtractSetupTransformationTest }.
	
	refactoring := self createRefactoringWithModel: model andArguments: { (14+12 to: 29+12) . #testExtractSetupExample5 . RBTestAsDataForExtractSetupTransformationTest}.
	self executeRefactoring: refactoring.
	class := refactoring model classNamed: #RBTestAsDataForExtractSetupTransformationTest.

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

{ #category : 'tests' }
RBExtractSetUpMethodParametrizedTest >> testExtractSetUpAndConvertTempsToInstVars [
	| class refactoring |
	model := self modelOnClasses: { TestCase . RBTestAsDataForExtractSetupTransformationTest }.
	
	refactoring := self createRefactoringWithModel: model andArguments: { (35+12  to: 81+12) . #testExtractSetupExample6 . RBTestAsDataForExtractSetupTransformationTest }.
	self executeRefactoring: refactoring.
	class := refactoring model classNamed: #RBTestAsDataForExtractSetupTransformationTest.
	self assert: (class instanceVariableNames includes: #aString2).
	
	"Pay attention that method body is created this way to support potential selector rename"
	self 
		assert: (class parseTreeForSelector: #testExtractSetupExample6) 
		equals: (self parseMethod: #testExtractSetupExample6 asString, Character cr asString,
'	| aNumber |
	aNumber := 4.
	self assert: aString2 isNotEmpty.
	self deny: (aString2 , aNumber asString) isEmpty.
	self assert: true').
	
	self 
		assert: (class parseTreeForSelector: #testExtractSetupExample7) 
		equals: (self parseMethod: #testExtractSetupExample7 asString, Character cr asString,
'	| aNumber |
	aString2 := ''sa''.
	self someMethod.
	aNumber := 4.
	self assert: aString2 isNotEmpty.').
	
	self 
		assert: (class parseTreeForSelector: #setUp) 
		equals: (self parseMethod: 'setUp
	super setUp.
	aString2 := ''Some string''.
	self someMethod.')
]

{ #category : 'tests' }
RBExtractSetUpMethodParametrizedTest >> testExtractSetUpAndConvertTwoTempsToInstVars [
	| class refactoring |
	self skip.
	refactoring := self createRefactoringWithArguments: { (47 to: 109) . #testExtractSetupExample6 . RBTestAsDataForExtractSetupTransformationTest }.
	self executeRefactoring: refactoring.
	class := refactoring model classNamed: #RBTestAsDataForExtractSetupTransformationTest.
	self assert: (class instanceVariableNames includes: #aString2).
	self assert: (class instanceVariableNames includes: #aNumber).
	
	"Pay attention that method body is created this way to support potential selector rename"
	
	self 
		assert: (class parseTreeForSelector: #testExtractSetupExample6) 
		equals: (self parseMethod: #testExtractSetupExample6 asString, Character cr asString,
'self assert: aString2 isNotEmpty.
	self deny: (aString2 , aNumber asString) isEmpty.
	self assert: true').
	
	
	self assert: (class parseTreeForSelector: #testExtractSetupExample7) equals: (self parseMethod: #testExtractSetupExample7 asString, Character cr asString,
'	aString2 := ''sa''.
	self someMethod.
	aNumber := 4.
	self assert: aString2 isNotEmpty.').
	
	"I do not get why the following is not working since the expression has been correctly removed from testExtractSetupExample6
	and in addition the tests with a single iv works and we have the two instance variables. 
	After trying during too long, I saw that the options of the refactorings can be changing (nautilus refactoring defines others and these are the ones
	used interactively. In addition randomly I got some do you want to search in the complete hierarchy."
	
	self 
		assert: (class parseTreeForSelector: #setUp) 
		equals: (self parseMethod: 'setUp
	super setUp.
	aString2 := ''Some string''.
	self someMethod.
	aNumber := 4')
]

{ #category : 'tests' }
RBExtractSetUpMethodParametrizedTest >> testExtractSetUpWorksWellInOtherMethod [
	| class refactoring |
	self skip.
	 
	"if we extract as Setup 
		 aString2 := 'Some string'.
		self someMethod.
	from method testExtractSetupExample6
	
	we get effectively and correctly the expression removed from the other methods such as 
	
		testExtractSetupExample8

		| aNumber |
		""this is an example to see if the extracted as setup expressions are also extracted from this one too.""
		aNumber := 4
	
	Now I do not succeed to convert this in a test.
	So I skip it for now.
	
	"
	refactoring := self createRefactoringWithArguments: { (47 to: 109) . #testExtractSetupExample6 . RBTestAsDataForExtractSetupTransformationTest }.
	self executeRefactoring: refactoring.
	class := refactoring model classNamed: #RBTestAsDataForExtractSetupTransformationTest.
	self assert: (class instanceVariableNames includes: #aString2).
	self assert: (class instanceVariableNames includes: #aNumber).
	
	"Pay attention that method body is created this way to support potential selector rename"
	
	self 
		assert: (class parseTreeForSelector: #testExtractSetupExample6) 
		equals: (self parseMethod: #testExtractSetupExample6 asString, Character cr asString,
'self assert: aString2 isNotEmpty.
	self deny: (aString2 , aNumber asString) isEmpty.
	self assert: true').
	
	"no idea what the extraction is not executed in testExtractSetupExample8 because when trying it works.
	This may be due to the fact that nautilus refactoring defines different options! "
	self 
		assert: (class parseTreeForSelector: #testExtractSetupExample8) 
		equals: (self parseMethod: #testExtractSetupExample8 asString, Character cr asString,
'	| aNumber |
	"this is an example to see if the extracted as setup expressions are also extracted from this one too."
	aNumber := 4').
	
	self assert: (class parseTreeForSelector: #setUp) equals: (self parseMethod: 'setUp
	super setUp.
	aString2 := ''Some string''.
	self someMethod.')
]

{ #category : 'failure tests' }
RBExtractSetUpMethodParametrizedTest >> testFailureBadClass [
	model := self modelOnClasses: { RBLintRuleTestData }.
	
	self shouldFail: (self createRefactoringWithModel: model andArguments: {
				 (78 to: 197).
				 #displayName.
				 RBLintRuleTestData })
]

{ #category : 'failure tests' }
RBExtractSetUpMethodParametrizedTest >> testFailureBadInterval [
	model := self modelOnClasses: { RBTestAsDataForExtractSetupTransformationTest }.
	
	self shouldFail: (self createRefactoringWithModel: model andArguments:
		{ (14 to: 35) . #testExtractSetupExample1 . RBTestAsDataForExtractSetupTransformationTest })
]

{ #category : 'failure tests' }
RBExtractSetUpMethodParametrizedTest >> testFailureExtractSetUpWhenIsNotFirstsSentences [
	model := self modelOnClasses: { RBTestAsDataForExtractSetupTransformationTest }.

	self shouldFail: (self createRefactoringWithModel: model andArguments: {
				 (63 to: 77).
				 #testExtractSetupExample1.
				 RBTestAsDataForExtractSetupTransformationTest })
]

{ #category : 'failure tests' }
RBExtractSetUpMethodParametrizedTest >> testFailureModelExistingSetUpMethod [

	| class |
	
	model := self modelOnClasses: { RBTestAsDataForExtractSetupTransformationTest }.
	
	class := model classNamed: #RBTestAsDataForExtractSetupTransformationTest.
	class compile: 'setUp #setUp' classified: #( #accessing ).
	self shouldFail:
		(self createRefactoringWithModel: model andArguments: {
				 (14 to: 29).
				 #testExtractSetupExample5.
				 RBTestAsDataForExtractSetupTransformationTest })
]
