Class {
	#name : 'RBExtractSetUpMethodAndOccurrencesParametrizedTest',
	#superclass : 'RBAbstractRefactoringTest',
	#category : 'Refactoring-Transformations-Tests-SingleParametrized',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'SingleParametrized'
}

{ #category : 'tests' }
RBExtractSetUpMethodAndOccurrencesParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBExtractSetUpMethodAndOccurrences };
		yourself
]

{ #category : 'accessing' }
RBExtractSetUpMethodAndOccurrencesParametrizedTest >> constructor [
	^ #extract:from:in:
]

{ #category : 'tests' }
RBExtractSetUpMethodAndOccurrencesParametrizedTest >> testExtractSetUpWorksWellInOtherMethod [

	| class refactoring |
	refactoring := self createRefactoringWithArguments: { (47 to: 109) . #testExtractSetupExample6 . RBTestAsDataForExtractSetupTransformationTest }.
	self setupSearchInAllHierarchyFor: refactoring toReturn: true.
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
	
	"This method was the same as setUp so now it is empty."
	self 
		assert: (class parseTreeForSelector: #testExtractSetupExample8) 
		equals: (self parseMethod: #testExtractSetupExample8 asString, Character cr asString,
'	"this is an example to see if the extracted as setup expressions are also extracted from this one too."
').
			
	self assert: (class parseTreeForSelector: #setUp) equals: (self parseMethod: 'setUp
	super setUp.
	aString2 := ''Some string''.
	self someMethod.
	aNumber := 4').

]

{ #category : 'tests' }
RBExtractSetUpMethodAndOccurrencesParametrizedTest >> testExtractSimpleMethodAndOcurrences [
	|class refactoring|
	refactoring := self createRefactoringWithArguments: { (15+12 to: 59+12) . #testExtractSetupExample1 . RBTestAsDataForExtractSetupTransformationTest }.
	self setupSearchInAllHierarchyFor: refactoring toReturn: true.
	self executeRefactoring: refactoring.
	class := refactoring model classNamed: #RBTestAsDataForExtractSetupTransformationTest.
	self assert: (class parseTreeForSelector: #setUp)
		equals: (self parseMethod: 'setUp
		super setUp. self accessToAClass. aString := ''Example''').
		
	self 
		assert: (class parseTreeForSelector: #testExtractSetupExample1)
		equals: (self parseMethod: #testExtractSetupExample1 asString, Character cr asString,
'		self assert: 4 > 5 equals: false').
	
	self 
		assert: (class parseTreeForSelector: #testExtractSetupExample2)
		equals: (self parseMethod: #testExtractSetupExample2 asString, Character cr asString,
'		self assert: true').
	
	self assert: (class parseTreeForSelector: #testExtractSetupExample3)
		equals: (self parseMethod: #testExtractSetupExample3 asString, Character cr asString,
'		self deny: false').
	
	self assert: (class parseTreeForSelector: #testExtractSetupExample4)
		equals: (self parseMethod: #testExtractSetupExample4 asString, Character cr asString,
'		self assert: true.
		self deny: false')
]

{ #category : 'failure tests' }
RBExtractSetUpMethodAndOccurrencesParametrizedTest >> testFailureBadInterval [
	| refactoring |
	refactoring := self createRefactoringWithArguments: { (36 to: 56) . #testExtractSetupExample1 . RBTestAsDataForExtractSetupTransformationTest }.
	self setupSearchInAllHierarchyFor: refactoring toReturn: true.
	self shouldFail: refactoring
]

{ #category : 'failure tests' }
RBExtractSetUpMethodAndOccurrencesParametrizedTest >> testFailureBadMethodName [

	| refactoring |
	refactoring := self createRefactoringWithArguments: {
			               (36 to: 56).
			               #testExample7.
			               RBTestAsDataForExtractSetupTransformationTest }.
	self setupSearchInAllHierarchyFor: refactoring toReturn: true.
	self shouldFail: refactoring
]
