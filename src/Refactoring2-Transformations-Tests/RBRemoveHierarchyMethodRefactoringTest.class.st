Class {
	#name : #RBRemoveHierarchyMethodRefactoringTest,
	#superclass : #RBAbstractTransformationTest,
	#category : #'Refactoring2-Transformations-Tests-Parametrized'
}

{ #category : #'failure tests' }
RBRemoveHierarchyMethodRefactoringTest >> testFailureRemoveNonExistantMethod [

	self shouldFail: (RBRemoveHierarchyMethodRefactoring
			 removeMethods: #( #nonExistingMethod )
			 from: RBBasicLintRuleTestData)
]

{ #category : #'failure tests' }
RBRemoveHierarchyMethodRefactoringTest >> testFailureRemoveReferenced [

	self shouldWarn: (RBRemoveHierarchyMethodRefactoring
				 removeMethods: #( #checkClass: )
				 from: RBBasicLintRuleTestData)
]

{ #category : #tests }
RBRemoveHierarchyMethodRefactoringTest >> testRemoveMethod [
	| refactoring selectors |
	selectors := Array with: 'msg4' asSymbol.
	refactoring := RBRemoveHierarchyMethodRefactoring removeMethods: selectors from: RBSharedPoolForTestData.
	self assert: ((refactoring model classNamed: #RBSharedPoolForTestData) directlyDefinesMethod: selectors first).
	self assert: ((refactoring model classNamed: #RBSharedPoolForTestData1) directlyDefinesMethod: selectors first).
	self assert: ((refactoring model classNamed: #RBSharedPoolForTestData2) directlyDefinesMethod: selectors first).
	refactoring generateChanges.
	self deny: ((refactoring model classNamed: #RBSharedPoolForTestData) directlyDefinesMethod: selectors first).
	self deny: ((refactoring model classNamed: #RBSharedPoolForTestData1) directlyDefinesMethod: selectors first).
	self deny: ((refactoring model classNamed: #RBSharedPoolForTestData2) directlyDefinesMethod: selectors first)
]
