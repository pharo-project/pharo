Class {
	#name : #RBRemoveClassKeepingSubclassesTest,
	#superclass : #RBRefactoringTest,
	#category : #'Refactoring-Tests-Core'
}

{ #category : #tests }
RBRemoveClassKeepingSubclassesTest >> testRemoveClassWithReferencesRaisesRBRefactoringError [
	self shouldFail: (RBRemoveClassRefactoring classNames: #(#RBBasicLintRuleTestData ))
]

{ #category : #tests }
RBRemoveClassKeepingSubclassesTest >> testRemoveNotEmptySuperclass [
	| refactoring class |
	refactoring := RBRemoveClassKeepingSubclassesRefactoring classNames: (Array with: ('RBTransformation' , 'RuleTestData1') asSymbol).
	class := refactoring model classNamed: ('RBTransformation' , 'RuleTestData2') asSymbol.
	self deny: (class directlyDefinesMethod: #foo).
	self executeRefactoring: refactoring.
	self assert: (refactoring model classNamed: ('RBTransformation' , 'RuleTestData1') asSymbol) isNil.
	self assert: (refactoring model classNamed: ('RBTransformation', 'RuleTestData2') 
	asSymbol) superclass equals: (refactoring model classNamed: #RBFooLintRuleTestData1).
	class := refactoring model classNamed: ('RBTransformation', 'RuleTestData2') 
	asSymbol.
	self assert: (class directlyDefinesMethod: #foo).
	
]
