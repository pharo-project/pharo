Class {
	#name : #RBInlineTemporaryParametrizedTest,
	#superclass : #RBAbstractRefactoringTest,
	#category : #'Refactoring2-Transformations-Tests-Parametrized'
}

{ #category : #tests }
RBInlineTemporaryParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBInlineTemporaryRefactoring };
		yourself
]

{ #category : #tests }
RBInlineTemporaryParametrizedTest >> testInlineTemporary [
	| refactoring |
	refactoring := rbClass
		inline: (24 to: 72)
		from: #inlineMethod
		in: RBRefactoryTestDataApp.
	
	self executeRefactoring: refactoring.

	self assert: ((refactoring model classNamed: #RBRefactoryTestDataApp) parseTreeFor: #inlineMethod) equals: (self parseMethod: 'inlineMethod
										^self
													foo;
													inlineMethod;
													bar')
]

{ #category : #'failure tests' }
RBInlineTemporaryParametrizedTest >> testInlineTemporaryBadInterval [
	self shouldFail: (rbClass
		inline: (29 to: 100)
		from: #moveDefinition
		in: RBRefactoryTestDataApp)
]

{ #category : #'failure tests' }
RBInlineTemporaryParametrizedTest >> testInlineTemporaryMutlipleAssignment [
	self shouldFail: (rbClass
		inline: (60 to: 83)
		from: #moveDefinition
		in: RBRefactoryTestDataApp)
]

{ #category : #'failure tests' }
RBInlineTemporaryParametrizedTest >> testInlineTemporaryReadBeforeWritten [
	self shouldFail: (rbClass
		inline: (48 to: 56)
		from: #inlineTemporary
		in: RBRefactoryTestDataApp)
]
