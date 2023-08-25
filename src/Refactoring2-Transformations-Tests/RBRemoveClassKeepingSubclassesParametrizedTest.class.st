Class {
	#name : #RBRemoveClassKeepingSubclassesParametrizedTest,
	#superclass : #RBAbstractRefactoringTest,
	#category : #'Refactoring2-Transformations-Tests-SingleParametrized'
}

{ #category : #tests }
RBRemoveClassKeepingSubclassesParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBRemoveClassPushingStateToSubclassesRefactoring };
		yourself
]

{ #category : #accessing }
RBRemoveClassKeepingSubclassesParametrizedTest >> constructor [
	^ #classNames:
]

{ #category : #tests }
RBRemoveClassKeepingSubclassesParametrizedTest >> testRemoveNotEmptySuperclassPushDownInstanceVariables [

	| refactoring subclass removedClassName subclassName |

	removedClassName := ('RBTransformation' , 'RuleTestData1') asSymbol.
	subclassName := ('RBTransformation' , 'RuleTestData2') asSymbol.
	refactoring := self createRefactoringWithArguments: { {removedClassName} }.
	subclass := refactoring model classNamed: subclassName.
	self deny: (subclass directlyDefinesMethod: #foo).

	self executeRefactoring: refactoring.

	self deny: (refactoring model includesClassNamed: removedClassName).
	self 
		assert: subclass superclass
		equals: (refactoring model classNamed: #RBFooLintRuleTestData1).
		
	self assert: (subclass directlyDefinesMethod: #foo).
	self assert: (subclass directlyDefinesInstanceVariable: #foo).
	self assert: (subclass directlyDefinesClassVariable: #A).
	
	
]

{ #category : #'failure tests' }
RBRemoveClassKeepingSubclassesParametrizedTest >> testShouldWarnWhenRemovingClassWithReferences [

	self shouldWarn: (self createRefactoringWithArguments:
			 { #( #RBBasicLintRuleTestData ) })
]
