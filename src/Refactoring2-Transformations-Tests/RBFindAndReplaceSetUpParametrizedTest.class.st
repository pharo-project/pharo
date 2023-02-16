Class {
	#name : #RBFindAndReplaceSetUpParametrizedTest,
	#superclass : #RBAbstractRefactoringTest,
	#category : #'Refactoring2-Transformations-Tests-SingleParametrized'
}

{ #category : #'building suites' }
RBFindAndReplaceSetUpParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBFindAndReplaceSetUpTransformation };
		yourself
]

{ #category : #accessing }
RBFindAndReplaceSetUpParametrizedTest >> constructor [
	^ #of:inWholeHierarchy:
]

{ #category : #tests }
RBFindAndReplaceSetUpParametrizedTest >> testClassDoesNotDefineSetUpMethod [
	| refactoring |
	refactoring := self createRefactoringWithArguments: { RBTest . true }.
	self shouldFail: refactoring
]

{ #category : #tests }
RBFindAndReplaceSetUpParametrizedTest >> testReplaceSetUp [
	| class refactoring |
	refactoring := self createRefactoringWithArguments: { RBTest . true }.
	class := refactoring model classNamed: #RBTest.
	class compile: 'setUp self someClasses. aString := ''Example''.'
		classified: #(#accessing).
	self executeRefactoring: refactoring.
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
