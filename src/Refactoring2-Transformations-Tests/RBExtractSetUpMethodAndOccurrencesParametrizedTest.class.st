Class {
	#name : #RBExtractSetUpMethodAndOccurrencesParametrizedTest,
	#superclass : #RBAbstractRefactoringTest,
	#category : #'Refactoring2-Transformations-Tests-SingleParametrized'
}

{ #category : #tests }
RBExtractSetUpMethodAndOccurrencesParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBExtractSetUpMethodAndOccurrences };
		yourself
]

{ #category : #accessing }
RBExtractSetUpMethodAndOccurrencesParametrizedTest >> constructor [
	^ #extract:from:in:
]

{ #category : #'failure tests' }
RBExtractSetUpMethodAndOccurrencesParametrizedTest >> testBadMethodName [
	| refactoring |
	refactoring := self createRefactoringWithArguments: { (36 to: 56) . #testExample7 . RBData2Test }.
	self setupSearchInAllHierarchyFor: refactoring toReturn: true.
	self shouldFail: refactoring
]

{ #category : #tests }
RBExtractSetUpMethodAndOccurrencesParametrizedTest >> testExtractSimpleMethodAndOcurrences [
	|class refactoring|
	refactoring := self createRefactoringWithArguments: { (15+12 to: 59+12) . #testExtractSetupExample1 . RBData2Test }.
	self setupSearchInAllHierarchyFor: refactoring toReturn: true.
	self executeRefactoring: refactoring.
	class := refactoring model classNamed: #RBData2Test.
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

{ #category : #'failure tests' }
RBExtractSetUpMethodAndOccurrencesParametrizedTest >> testFailureBadInterval [
	| refactoring |
	refactoring := self createRefactoringWithArguments: { (36 to: 56) . #testExtractSetupExample1 . RBData2Test }.
	self setupSearchInAllHierarchyFor: refactoring toReturn: true.
	self shouldFail: refactoring
]

{ #category : #'failure tests' }
RBExtractSetUpMethodAndOccurrencesParametrizedTest >> testFailureBadMethodName [

	| refactoring |
	refactoring := self createRefactoringWithArguments: {
			               (36 to: 56).
			               #testExample7.
			               RBData2Test }.
	self setupSearchInAllHierarchyFor: refactoring toReturn: true.
	self shouldFail: refactoring
]
