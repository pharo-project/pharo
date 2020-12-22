Class {
	#name : #RBRemoveHierarchyMethodTest,
	#superclass : #RBRefactoringTest,
	#category : #'Refactoring-Tests-Core-Refactorings'
}

{ #category : #tests }
RBRemoveHierarchyMethodTest >> testRemoveMethod [
	| refactoring selectors |
	selectors := Array with: 'msg4' asSymbol.
	refactoring := RBRemoveHierarchyMethodRefactoring 
		removeMethods: selectors
		from: RBSharedPoolForTestData.
	self assert: ((refactoring model classNamed: #RBSharedPoolForTestData) directlyDefinesMethod: selectors first).
	self assert: ((refactoring model classNamed: #RBSharedPoolForTestData1) directlyDefinesMethod: selectors first).
	self assert: ((refactoring model classNamed: #RBSharedPoolForTestData2) directlyDefinesMethod: selectors first).
	self executeRefactoring: refactoring.
	self deny: ((refactoring model classNamed: #RBSharedPoolForTestData) directlyDefinesMethod: selectors first).
	self deny: ((refactoring model classNamed: #RBSharedPoolForTestData1) directlyDefinesMethod: selectors first).
	self deny: ((refactoring model classNamed: #RBSharedPoolForTestData2) directlyDefinesMethod: selectors first)
]

{ #category : #'failure tests' }
RBRemoveHierarchyMethodTest >> testRemoveNonExistantMethod [
	self shouldFail: (RBRemoveHierarchyMethodRefactoring 
			removeMethods: #(#msg7 )
			from: RBBasicLintRuleTestData)
]

{ #category : #'failure tests' }
RBRemoveHierarchyMethodTest >> testRemoveReferenced [
	self shouldFail: (RBRemoveHierarchyMethodRefactoring 
			removeMethods: #(#msg1 )
			from: RBBasicLintRuleTestData)
]
