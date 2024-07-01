Class {
	#name : 'RBRemoveClassPushStateToSubclassesTest',
	#superclass : 'RBAbstractTransformationTest',
	#category : 'Refactoring-Transformations-Tests-Test',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Test'
}

{ #category : 'tests' }
RBRemoveClassPushStateToSubclassesTest >> testRemoveNotEmptySuperclassPushDownInstanceVariables [

	| refactoring subclass removedClassName subclassName |
	removedClassName := ('RBTransformation' , 'RuleTestData1') asSymbol.
	subclassName := ('RBTransformation' , 'RuleTestData2') asSymbol.
	model := self modelOnClasses: {
			         (Smalltalk classNamed: removedClassName).
			         (Smalltalk classNamed: subclassName).
			         RBFooLintRuleTestData1 }.
	refactoring := ReRemoveClassPushingStateToSubclassesRefactoring
		               model: model
		               classNames: { removedClassName }.
	subclass := refactoring model classNamed: subclassName.
	self deny: (subclass directlyDefinesMethod: #foo).

	refactoring generateChanges.

	self deny: (refactoring model includesClassNamed: removedClassName).
	self
		assert: subclass superclass
		equals: (refactoring model classNamed: #RBFooLintRuleTestData1).

	self assert: (subclass directlyDefinesMethod: #foo).
	self assert: (subclass directlyDefinesInstanceVariable: #foo).
	self assert: (subclass directlyDefinesClassVariable: #A)
]

{ #category : 'tests' }
RBRemoveClassPushStateToSubclassesTest >> testShouldWarnWhenRemovingClassWithReferences [

	| refactoring |
	model := self modelOnClasses: { RBBasicLintRuleTestData . self class }.
	
	refactoring := ReRemoveClassPushingStateToSubclassesRefactoring model: model classNames: { #RBBasicLintRuleTestData }.
	"self class refers to RBBasicLintRuleTestData"
	self
		should: [ refactoring generateChanges ]
		raise: RBRefactoringWarning
]
