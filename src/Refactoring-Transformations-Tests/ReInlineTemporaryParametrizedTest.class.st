Class {
	#name : 'ReInlineTemporaryParametrizedTest',
	#superclass : 'RBAbstractRefactoringTest',
	#category : 'Refactoring-Transformations-Tests-Parametrized',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Parametrized'
}

{ #category : 'tests' }
ReInlineTemporaryParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> ReInlineTemporaryRefactoring };
		yourself
]

{ #category : 'accessing' }
ReInlineTemporaryParametrizedTest >> constructor [
	^ #inline:from:in:
]

{ #category : 'failure tests' }
ReInlineTemporaryParametrizedTest >> testFailureInlineTemporaryBadInterval [

	self shouldFail: (self createRefactoringWithArguments: {
				 (29 to: 100).
				 #moveDefinition.
				 RBClassDataForRefactoringTest })
]

{ #category : 'failure tests' }
ReInlineTemporaryParametrizedTest >> testFailureInlineTemporaryMultipleRead [

	self shouldFail: (self createRefactoringWithArguments: {
				 (38 to: 62).
				 #inlineFoo:.
				 RBClassDataForRefactoringTest })
]

{ #category : 'failure tests' }
ReInlineTemporaryParametrizedTest >> testFailureInlineTemporaryMutlipleAssignment [

	self shouldFail: (self createRefactoringWithArguments: {
				 (58 to: 81).
				 #moveDefinition.
				 RBClassDataForRefactoringTest })
]

{ #category : 'failure tests' }
ReInlineTemporaryParametrizedTest >> testFailureInlineTemporaryReadBeforeWritten [

	self shouldFail: (self createRefactoringWithArguments: {
				 (48 to: 56).
				 #inlineTemporary.
				 RBClassDataForRefactoringTest })
]

{ #category : 'tests' }
ReInlineTemporaryParametrizedTest >> testInlineTemporary [
	| refactoring |
	refactoring := self createRefactoringWithArguments:
		{ (24 to: 72) . #inlineMethod . RBClassDataForRefactoringTest }.

	self executeRefactoring: refactoring.

	self assert: ((refactoring model classNamed: #RBClassDataForRefactoringTest) parseTreeForSelector: #inlineMethod) equals: (self parseMethod: 'inlineMethod
										^self
													foo;
													inlineMethod;
													bar')
]
