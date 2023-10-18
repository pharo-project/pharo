Class {
	#name : 'RBInlineTemporaryParametrizedTest',
	#superclass : 'RBAbstractRefactoringTest',
	#category : 'Refactoring-Transformations-Tests-Parametrized',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Parametrized'
}

{ #category : 'tests' }
RBInlineTemporaryParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBInlineTemporaryRefactoring };
		addCase: { #rbClass -> RBInlineTemporaryTransformation };
		yourself
]

{ #category : 'accessing' }
RBInlineTemporaryParametrizedTest >> constructor [
	^ #inline:from:in:
]

{ #category : 'failure tests' }
RBInlineTemporaryParametrizedTest >> testFailureInlineTemporaryBadInterval [

	self shouldFail: (self createRefactoringWithArguments: {
				 (29 to: 100).
				 #moveDefinition.
				 RBClassDataForRefactoringTest })
]

{ #category : 'failure tests' }
RBInlineTemporaryParametrizedTest >> testFailureInlineTemporaryMutlipleAssignment [

	self shouldFail: (self createRefactoringWithArguments: {
				 (58 to: 81).
				 #moveDefinition.
				 RBClassDataForRefactoringTest })
]

{ #category : 'failure tests' }
RBInlineTemporaryParametrizedTest >> testFailureInlineTemporaryReadBeforeWritten [

	self shouldFail: (self createRefactoringWithArguments: {
				 (48 to: 56).
				 #inlineTemporary.
				 RBClassDataForRefactoringTest })
]

{ #category : 'tests' }
RBInlineTemporaryParametrizedTest >> testInlineTemporary [
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
