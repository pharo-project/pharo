Class {
	#name : #RBRemoveClassTest,
	#superclass : #RBRefactoringTest,
	#category : #'Refactoring-Tests-Core'
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
RBRemoveClassTest >> testRemoveAClassAndTheirSubclass [
	|refactoring|
	
	refactoring := RBRemoveClassRefactoring classNames: {#RBSharedPoolForTestData1 . #RBSharedPoolForTestData2}.
	self executeRefactoring: refactoring.
	self assert: (refactoring model classNamed: #RBSharedPoolForTestData2) isNil.
	self assert: (refactoring model classNamed: #RBSharedPoolForTestData1) isNil.
]

{ #category : #'failure tests' }
RBRemoveClassTest >> testRemoveClassWithBadNameRaisesRBRefactoringError [ 
	self 
		shouldFail: (RBRemoveClassRefactoring classNames: #(#RecursiveSelfRule))
]

{ #category : #'failure tests' }
RBRemoveClassTest >> testRemoveClassWithReferencesRaisesRBRefactoringError [
	self shouldFail: (RBRemoveClassRefactoring classNames: #(#RBBasicLintRuleTestData ))
]

{ #category : #'failure tests' }
RBRemoveClassTest >> testRemoveClassesWithReferencesBetweenThem [
	|refactoring|
	
	refactoring := RBRemoveClassRefactoring classNames: {#RBRemoveClassTest  . #RBSharedPoolForTestData2}.
	self executeRefactoring: refactoring.
	self assert: (refactoring model classNamed: #RBRemoveClassTest) isNil.
	self assert: (refactoring model classNamed: #RBSharedPoolForTestData2) isNil.
]

{ #category : #tests }
RBRemoveClassTest >> testRemoveEmptySuperclass [
	| refactoring |
	refactoring := RBRemoveClassRefactoring classNames: (Array with: ('RBFoo' , 'LintRuleTestData1') asSymbol).
	self executeRefactoring: refactoring.
	self assert: (refactoring model classNamed: ('RBFoo' , 'LintRuleTestData1') asSymbol) isNil.
	self assert: (refactoring model classNamed: #RBTransformationRuleTestData1) superclass equals: (refactoring model classNamed: #RBLintRuleTestData)
]
