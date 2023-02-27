Class {
	#name : #RBRemoveHierarchyMethodParametrizedTest,
	#superclass : #RBAbstractRefactoringTest,
	#category : #'Refactoring2-Transformations-Tests-Parametrized'
}

{ #category : #tests }
RBRemoveHierarchyMethodParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBRemoveHierarchyMethodRefactoring };
		addCase: { #rbClass -> RBRemoveHierarchyMethodTransformation };
		yourself
]

{ #category : #accessing }
RBRemoveHierarchyMethodParametrizedTest >> constructor [
	^ #removeMethods:from:
]

{ #category : #tests }
RBRemoveHierarchyMethodParametrizedTest >> testRemoveMethod [
	| refactoring selectors |
	selectors := Array with: 'msg4' asSymbol.
	refactoring := self createRefactoringWithArguments: { selectors . RBSharedPoolForTestData }.
	self assert: ((refactoring model classNamed: #RBSharedPoolForTestData) directlyDefinesMethod: selectors first).
	self assert: ((refactoring model classNamed: #RBSharedPoolForTestData1) directlyDefinesMethod: selectors first).
	self assert: ((refactoring model classNamed: #RBSharedPoolForTestData2) directlyDefinesMethod: selectors first).
	self executeRefactoring: refactoring.
	self deny: ((refactoring model classNamed: #RBSharedPoolForTestData) directlyDefinesMethod: selectors first).
	self deny: ((refactoring model classNamed: #RBSharedPoolForTestData1) directlyDefinesMethod: selectors first).
	self deny: ((refactoring model classNamed: #RBSharedPoolForTestData2) directlyDefinesMethod: selectors first)
]

{ #category : #'failure tests' }
RBRemoveHierarchyMethodParametrizedTest >> testRemoveNonExistantMethod [
	self shouldFail: (self createRefactoringWithArguments: { #(#msg7) . RBBasicLintRuleTestData})
]

{ #category : #'failure tests' }
RBRemoveHierarchyMethodParametrizedTest >> testRemoveReferenced [
	self shouldFail: (self createRefactoringWithArguments: { #(#msg1) . RBBasicLintRuleTestData })
]
