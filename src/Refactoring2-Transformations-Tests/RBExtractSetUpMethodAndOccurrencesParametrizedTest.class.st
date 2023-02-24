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
RBExtractSetUpMethodAndOccurrencesParametrizedTest >> testBadInterval [
	| refactoring |
	refactoring := self createRefactoringWithArguments: { (36 to: 56) . #testExample1 . RBTest }.
	self setupSearchInAllHierarchyFor: refactoring toReturn: true.
	self shouldFail: refactoring
]

{ #category : #'failure tests' }
RBExtractSetUpMethodAndOccurrencesParametrizedTest >> testBadMethodName [
	| refactoring |
	refactoring := self createRefactoringWithArguments: { (36 to: 56) . #testExample7 . RBTest }.
	self setupSearchInAllHierarchyFor: refactoring toReturn: true.
	self shouldFail: refactoring
]

{ #category : #tests }
RBExtractSetUpMethodAndOccurrencesParametrizedTest >> testExtractSimpleMethodAndOcurrences [
	|class refactoring|
	refactoring := self createRefactoringWithArguments: { (15 to: 54) . #testExample1 . RBTest }.
	self setupSearchInAllHierarchyFor: refactoring toReturn: true.
	self executeRefactoring: refactoring.
	class := refactoring model classNamed: #RBTest.
	self assert: (class parseTreeForSelector: #setUp)
		equals: (self parseMethod: 'setUp
		super setUp. self someClasses. aString := ''Example''').
	self assert: (class parseTreeForSelector: #testExample1)
		equals: (self parseMethod: 'testExample1
		self assert: 4 > 5 equals: false').
	self assert: (class parseTreeForSelector: #testExample2)
		equals: (self parseMethod: 'testExample2
		self assert: true').
	self assert: (class parseTreeForSelector: #testExample3)
		equals: (self parseMethod: 'testExample3
		self deny: false').
	self assert: (class parseTreeForSelector: #testExample4)
		equals: (self parseMethod: 'testExample4
		self assert: true.
		self deny: false')
]
