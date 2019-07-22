Class {
	#name : #RBInlineTemporaryTest,
	#superclass : #RBRefactoringTest,
	#category : #'Refactoring-Tests-Core'
}

{ #category : #tests }
RBInlineTemporaryTest >> testInlineTemporary [
	| refactoring |
	refactoring := RBInlineTemporaryRefactoring
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

{ #category : #tests }
RBInlineTemporaryTest >> testInlineTemporaryBadInterval [
	self
		shouldFail:
			(RBInlineTemporaryRefactoring
				inline: (29 to: 100)
				from: #moveDefinition
				in: RBRefactoryTestDataApp)
]

{ #category : #tests }
RBInlineTemporaryTest >> testInlineTemporaryMutlipleAssignment [
	self
		shouldFail:
			(RBInlineTemporaryRefactoring
				inline: (60 to: 83)
				from: #moveDefinition
				in: RBRefactoryTestDataApp)
]

{ #category : #tests }
RBInlineTemporaryTest >> testInlineTemporaryReadBeforeWritten [
	self
		shouldFail:
			(RBInlineTemporaryRefactoring
				inline: (48 to: 56)
				from: #inlineTemporary
				in: RBRefactoryTestDataApp)
]
