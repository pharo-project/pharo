Class {
	#name : 'RBFindAndReplaceSetUpParametrizedTest',
	#superclass : 'RBAbstractRefactoringTest',
	#category : 'Refactoring-Transformations-Tests-SingleParametrized',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'SingleParametrized'
}

{ #category : 'building suites' }
RBFindAndReplaceSetUpParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBFindAndReplaceSetUpTransformation };
		yourself
]

{ #category : 'accessing' }
RBFindAndReplaceSetUpParametrizedTest >> constructor [
	^ #of:inWholeHierarchy:
]

{ #category : 'tests' }
RBFindAndReplaceSetUpParametrizedTest >> testClassDoesNotDefineSetUpMethod [
	| refactoring |
	refactoring := self createRefactoringWithArguments: { RBTestAsDataForExtractSetupTransformationTest . true }.
	self shouldFail: refactoring
]

{ #category : 'tests' }
RBFindAndReplaceSetUpParametrizedTest >> testReplaceSetUp [
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
