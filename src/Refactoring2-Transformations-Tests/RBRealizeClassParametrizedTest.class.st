Class {
	#name : #RBRealizeClassParametrizedTest,
	#superclass : #RBAbstractRefactoringTest,
	#category : #'Refactoring2-Transformations-Tests-Parametrized'
}

{ #category : #tests }
RBRealizeClassParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBRealizeClassRefactoring };
		yourself
]

{ #category : #builder }
RBRealizeClassParametrizedTest >> createAbstractClass [ 
	(model classNamed: #RBFooLintRuleTestData) 
		compile: 'bar ^ self subclassResponsibility'
		classified: #(#accessing).
	(model classNamed: #RBFooLintRuleTestData) classSide
		compile: 'foo ^ self subclassResponsibility'
		classified: #(#accessing).
	
	
]

{ #category : #tests }
RBRealizeClassParametrizedTest >> testClassWithoutChanges [
	self proceedThroughWarning: [ self executeRefactoring: (rbClass 
		model: model 
		className: #RBLintRuleTestData) asRefactoring ].
	self assert: model changes changes isEmpty
]

{ #category : #tests }
RBRealizeClassParametrizedTest >> testRealizeAbstractClass [
	| class |
	class := model classNamed: #RBFooLintRuleTestData.
	self createAbstractClass.
	self proceedThroughWarning: [ 
		self executeRefactoring: (rbClass 
			model: model 
			className: #RBFooLintRuleTestData) asRefactoring ].
	self assert: (class parseTreeFor: #isEmpty) equals: 
	(self parseMethod: 'isEmpty
		self shouldBeImplemented').
	self assert: (class parseTreeFor: #problemCount) equals: 
	(self parseMethod: 'problemCount
		self shouldBeImplemented').
	self assert: (class parseTreeFor: #viewResults) equals: 
	(self parseMethod: 'viewResults
		self shouldBeImplemented') 
]

{ #category : #tests }
RBRealizeClassParametrizedTest >> testRealizeClass [
	| class |
	class := model classNamed: #RBFooLintRuleTestData.
	self executeRefactoring: (rbClass 
		model: model 
		className: #RBFooLintRuleTestData) asRefactoring.
	self assert: (class parseTreeFor: #isEmpty) equals: 
	(self parseMethod: 'isEmpty
		self shouldBeImplemented').
	self assert: (class parseTreeFor: #problemCount) equals: 
	(self parseMethod: 'problemCount
		self shouldBeImplemented').
	self assert: (class parseTreeFor: #viewResults) equals: 
	(self parseMethod: 'viewResults
		self shouldBeImplemented') 
]

{ #category : #tests }
RBRealizeClassParametrizedTest >> testRealizeWithAbstractSubclass [
	| class |
	class := model classNamed: #RBTransformationRuleTestData.
	class compile: 'foo: arg1 bar: arg2 self subclassResponsibility' classified: 'accessing'.
	self proceedThroughWarning: [ 
		self executeRefactoring: (rbClass 
			model: model 
			className: #RBFooLintRuleTestData) asRefactoring ].
	class := model classNamed: #RBFooLintRuleTestData.
	self assert: (class parseTreeFor: #isEmpty) equals: 
	(self parseMethod: 'isEmpty
		self shouldBeImplemented').
	self assert: (class parseTreeFor: #problemCount) equals: 
	(self parseMethod: 'problemCount
		self shouldBeImplemented').
	self assert: (class parseTreeFor: #viewResults) equals: 
	(self parseMethod: 'viewResults
		self shouldBeImplemented') 
]
