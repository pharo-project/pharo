Class {
	#name : #RBRemoveClassTest,
	#superclass : #RBRefactoringTest,
	#category : #'Refactoring-Tests-Core-Refactorings'
}

{ #category : #private }
RBRemoveClassTest >> resumeOnCannotRemoveTopLevelClassError: error [
	^ error messageText = RBRemoveClassRefactoring cannotRemoveTopLevelClassErrorMesssage
		ifTrue: [ error resume ]
]

{ #category : #'failure tests' }
RBRemoveClassTest >> testRaisesRBRefactoringErrorWhenRemovingNonEmptySuperclass [
	|class|
	class := ('RBTransformation', 'RuleTestData1') asSymbol.
	self shouldFail: (RBRemoveClassRefactoring classNames: {class})
]

{ #category : #tests }
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

{ #category : #tests }
RBRemoveClassTest >> testRemoveClassesWithReferencesBetweenThem [
	|refactoring|
	
	refactoring := RBRemoveClassRefactoring classNames: {#RBRemoveClassTest  . #RBSharedPoolForTestData2}.
	self executeRefactoring: refactoring.
	self assert: (refactoring model classNamed: #RBRemoveClassTest) isNil.
	self assert: (refactoring model classNamed: #RBSharedPoolForTestData2) isNil.
]

{ #category : #tests }
RBRemoveClassTest >> testRemoveEmptySuperclass [
	| refactoring classA classB classC |
	
	classA := ('RBFoo' , 'LintRuleTestData1') asSymbol.
	classB := ('RBTransformation' , 'RuleTestData1') asSymbol.
	classC := #RBLintRuleTestData.
	refactoring := RBRemoveClassRefactoring classNames: {classA}.
	self executeRefactoring: refactoring.
	self assert: (refactoring model classNamed: classA) isNil.
	self assert: (refactoring model classNamed: classB) superclass equals: (refactoring model classNamed: classC)
]
