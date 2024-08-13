Class {
	#name : 'RBRealizeClassParametrizedTest',
	#superclass : 'RBAbstractRefactoringTest',
	#category : 'Refactoring-Transformations-Tests-SingleParametrized',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'SingleParametrized'
}

{ #category : 'building suites' }
RBRealizeClassParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBRealizeClassTransformation };
		yourself
]

{ #category : 'builder' }
RBRealizeClassParametrizedTest >> addSomeAbstractMethods [
	(model classNamed: #RBFooLintRuleTestData)
		compile: 'bar ^ self subclassResponsibility'
		classified: #(#accessing).
	(model classNamed: #RBFooLintRuleTestData) classSide
		compile: 'foo ^ self subclassResponsibility'
		classified: #(#accessing)
]

{ #category : 'accessing' }
RBRealizeClassParametrizedTest >> constructor [
	^ #model:className:
]

{ #category : 'tests' }
RBRealizeClassParametrizedTest >> testClassWithoutChanges [
	self proceedThroughWarning: [
		self executeRefactoring:
			(self createRefactoringWithArguments: { model . #RBLintRuleTestData}) ].
	self assert: model changes changes isEmpty
]

{ #category : 'tests' }
RBRealizeClassParametrizedTest >> testRealizeAbstractClass [
	| class |
	class := model classNamed: #RBFooLintRuleTestData.
	self addSomeAbstractMethods.
	self proceedThroughWarning: [
		self executeRefactoring:
			(self createRefactoringWithArguments:  { model . #RBFooLintRuleTestData}) ].
	self assert: (class parseTreeForSelector: #isEmpty) equals:
	(self parseMethod: 'isEmpty
		self shouldBeImplemented').
	self assert: (class parseTreeForSelector: #problemCount) equals:
	(self parseMethod: 'problemCount
		self shouldBeImplemented').
	self assert: (class parseTreeForSelector: #viewResults) equals:
	(self parseMethod: 'viewResults
		self shouldBeImplemented')
]

{ #category : 'tests' }
RBRealizeClassParametrizedTest >> testRealizeClass [
	| class |
	class := model classNamed: #RBFooLintRuleTestData.
	self executeRefactoring:
		(self createRefactoringWithArguments:  { model . #RBFooLintRuleTestData }).
	self assert: (class parseTreeForSelector: #isEmpty) equals:
	(self parseMethod: 'isEmpty
		self shouldBeImplemented').
	self assert: (class parseTreeForSelector: #problemCount) equals:
	(self parseMethod: 'problemCount
		self shouldBeImplemented').
	self assert: (class parseTreeForSelector: #viewResults) equals:
	(self parseMethod: 'viewResults
		self shouldBeImplemented')
]

{ #category : 'tests' }
RBRealizeClassParametrizedTest >> testRealizeWithAbstractSubclass [
	| class |
	class := model classNamed: #RBTransformationRuleTestData.
	class compile: 'foo: arg1 bar: arg2 self subclassResponsibility' classified: 'accessing'.
	self proceedThroughWarning: [
		self executeRefactoring:
			(self createRefactoringWithArguments: { model . #RBFooLintRuleTestData }) ].
	class := model classNamed: #RBFooLintRuleTestData.
	self assert: (class parseTreeForSelector: #isEmpty) equals:
	(self parseMethod: 'isEmpty
		self shouldBeImplemented').
	self assert: (class parseTreeForSelector: #problemCount) equals:
	(self parseMethod: 'problemCount
		self shouldBeImplemented').
	self assert: (class parseTreeForSelector: #viewResults) equals:
	(self parseMethod: 'viewResults
		self shouldBeImplemented')
]
