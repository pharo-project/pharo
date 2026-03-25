Class {
	#name : 'RBFindAndReplaceSetUpTest',
	#superclass : 'RBAbstractRefactoringTest',
	#category : 'Refactoring-Transformations-Tests-SingleParametrized',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'SingleParametrized'
}

{ #category : 'accessing' }
RBFindAndReplaceSetUpTest >> constructor [
	^ #of:inWholeHierarchy:
]

{ #category : 'running' }
RBFindAndReplaceSetUpTest >> setUp [ 

	super setUp.
	rbClass := RBFindAndReplaceSetUpTransformation
]

{ #category : 'tests' }
RBFindAndReplaceSetUpTest >> testClassDoesNotDefineSetUpMethod [
	| refactoring |
	refactoring := self createRefactoringWithArguments: { RBTestAsDataForExtractSetupTransformationTest . true }.
	self shouldFail: refactoring

]

{ #category : 'tests' }
RBFindAndReplaceSetUpTest >> testReplaceSetUp [
	| class refactoring |
	refactoring := self createRefactoringWithArguments: { RBTestAsDataForExtractSetupTransformationTest . true }.
	class := refactoring model classNamed: #RBTestAsDataForExtractSetupTransformationTest.
	class compile: 'setUp self accessToAClass. aString := ''Example''.'
		classified: #(#accessing).
	self executeRefactoring: refactoring.
	self assert: (class parseTreeForSelector: #testExtractSetupExample1)
		equals: (self parseMethod: 'testExtractSetupExample1
		self assert: 4 > 5 equals: false').
	self assert: (class parseTreeForSelector: #testExtractSetupExample2)
		equals: (self parseMethod: 'testExtractSetupExample2
		self assert: true').
	self assert: (class parseTreeForSelector: #testExtractSetupExample3)
		equals: (self parseMethod: 'testExtractSetupExample3
		self deny: false').
	self assert: (class parseTreeForSelector: #testExtractSetupExample4)
		equals: (self parseMethod: 'testExtractSetupExample4
		self assert: true.
		self deny: false')
]
