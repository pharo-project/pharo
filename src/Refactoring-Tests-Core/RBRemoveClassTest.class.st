Class {
	#name : #RBRemoveClassTest,
	#superclass : #RBRefactoringTest,
	#category : 'Refactoring-Tests-Core'
}

{ #category : #private }
RBRemoveClassTest >> resumeOnCannotRemoveTopLevelClassError: error [
	^ error messageText = RBRemoveClassRefactoring cannotRemoveTopLevelClassErrorMesssage
		ifTrue: [ error resume ]
]

{ #category : #tests }
RBRemoveClassTest >> testRaisesRBRefactoringErrorWhenRemovingNonEmptySuperclass [
	self shouldFail: (RBRemoveClassRefactoring classNames: (Array with: ('RBFoo' , 'LintRuleTest') asSymbol))
]

{ #category : #'failure tests' }
RBRemoveClassTest >> testRemoveClassWithBadNameRaisesRBRefactoringError [ 
	self 
		shouldFail: (RBRemoveClassRefactoring classNames: #(#RecursiveSelfRule))
]

{ #category : #'failure tests' }
RBRemoveClassTest >> testRemoveClassWithReferencesRaisesRBRefactoringError [
	self shouldFail: (RBRemoveClassRefactoring classNames: #(#RBBasicLintRuleTest ))
]

{ #category : #tests }
RBRemoveClassTest >> testRemoveEmptySuperclass [
	| refactoring |
	refactoring := RBRemoveClassRefactoring classNames: (Array with: ('RBFoo' , 'LintRuleTest1') asSymbol).
	self executeRefactoring: refactoring.
	self assert: (refactoring model classNamed: ('RBFoo' , 'LintRuleTest1') asSymbol) isNil.
	self assert: (refactoring model classNamed: #RBTransformationRuleTest1) superclass = (refactoring model classNamed: #RBLintRuleTest)
]
