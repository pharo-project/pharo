Class {
	#name : 'RBRemoveClassPushStateToSubclassesTest',
	#superclass : 'RBAbstractTransformationTest',
	#category : 'Refactoring2-Transformations-Tests-Test',
	#package : 'Refactoring2-Transformations-Tests',
	#tag : 'Test'
}

{ #category : 'tests' }
RBRemoveClassPushStateToSubclassesTest >> testRemoveNotEmptySuperclassPushDownInstanceVariables [

	| refactoring subclass removedClassName subclassName |

	removedClassName := ('RBTransformation' , 'RuleTestData1') asSymbol.
	subclassName := ('RBTransformation' , 'RuleTestData2') asSymbol.
	refactoring := RBRemoveClassPushingStateToSubclassesRefactoring classNames: {removedClassName} .
	subclass := refactoring model classNamed: subclassName.
	self deny: (subclass directlyDefinesMethod: #foo).

	refactoring generateChanges.

	self deny: (refactoring model includesClassNamed: removedClassName).
	self 
		assert: subclass superclass
		equals: (refactoring model classNamed: #RBFooLintRuleTestData1).
		
	self assert: (subclass directlyDefinesMethod: #foo).
	self assert: (subclass directlyDefinesInstanceVariable: #foo).
	self assert: (subclass directlyDefinesClassVariable: #A).
	
	
]

{ #category : 'tests' }
RBRemoveClassPushStateToSubclassesTest >> testShouldWarnWhenRemovingClassWithReferences [

	| refactoring |
	refactoring := RBRemoveClassPushingStateToSubclassesRefactoring classNames: { #RBBasicLintRuleTestData }.
	self
		should: [ refactoring generateChanges ]
		raise: RBRefactoringWarning 
]
