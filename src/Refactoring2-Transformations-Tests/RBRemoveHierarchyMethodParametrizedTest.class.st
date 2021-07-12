Class {
	#name : #RBRemoveHierarchyMethodParametrizedTest,
	#superclass : #RBAbstractRefactoringTest,
	#category : #'Refactoring2-Transformations-Tests-Parametrized'
}

{ #category : #tests }
RBRemoveHierarchyMethodParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBRemoveHierarchyMethodRefactoring };
		yourself
]

{ #category : #tests }
RBRemoveHierarchyMethodParametrizedTest >> testRemoveMethod [
	| refactoring selectors |
	selectors := Array with: 'msg4' asSymbol.
	refactoring := rbClass 
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
RBRemoveHierarchyMethodParametrizedTest >> testRemoveNonExistantMethod [
	self shouldFail: (rbClass 
			removeMethods: #(#msg7 )
			from: RBBasicLintRuleTestData)
]

{ #category : #'failure tests' }
RBRemoveHierarchyMethodParametrizedTest >> testRemoveReferenced [
	self shouldFail: (rbClass 
			removeMethods: #(#msg1 )
			from: RBBasicLintRuleTestData)
]
