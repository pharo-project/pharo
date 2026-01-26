Class {
	#name : 'RBRemoveMethodsInHierarchyRefactoringTest',
	#superclass : 'RBAbstractTransformationTest',
	#category : 'Refactoring-Transformations-Tests-Test',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Test'
}

{ #category : 'failure tests' }
RBRemoveMethodsInHierarchyRefactoringTest >> testFailureRemoveNonExistantMethod [

	self shouldFail: (ReRemoveMethodsInHierarchyRefactoring
			 selectors: #( #nonExistingMethod )
			 from: RBBasicLintRuleTestData)
]

{ #category : 'failure tests' }
RBRemoveMethodsInHierarchyRefactoringTest >> testFailureRemoveReferenced [

	self shouldWarn: (ReRemoveMethodsInHierarchyRefactoring
				 selectors: #( #checkClass: )
				 from: RBBasicLintRuleTestData)
]

{ #category : 'tests' }
RBRemoveMethodsInHierarchyRefactoringTest >> testRemoveMethod [
	| refactoring selectors |
	selectors := Array with: 'msg4' asSymbol.
	refactoring := ReRemoveMethodsInHierarchyRefactoring selectors: selectors from: RBSharedPoolForTestData.
	self assert: ((refactoring model classNamed: #RBSharedPoolForTestData) directlyDefinesMethod: selectors first).
	self assert: ((refactoring model classNamed: #RBSharedPoolForTestData1) directlyDefinesMethod: selectors first).
	self assert: ((refactoring model classNamed: #RBSharedPoolForTestData2) directlyDefinesMethod: selectors first).
	refactoring generateChanges.
	self deny: ((refactoring model classNamed: #RBSharedPoolForTestData) directlyDefinesMethod: selectors first).
	self deny: ((refactoring model classNamed: #RBSharedPoolForTestData1) directlyDefinesMethod: selectors first).
	self deny: ((refactoring model classNamed: #RBSharedPoolForTestData2) directlyDefinesMethod: selectors first)
]
