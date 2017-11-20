Class {
	#name : #RBInlineTemporaryTest,
	#superclass : #RBRefactoringTest,
	#category : #Refactoring-Tests-Core
}

{ #category : #tests }
RBInlineTemporaryTest >> testInlineTemporary [
	| refactoring |
	refactoring := RBInlineTemporaryRefactoring 
		inline: (self 
				convertInterval: (24 to: 72)
				for: (RBRefactoryTestDataApp sourceCodeAt: #inlineMethod))
		from: #inlineMethod
		in: RBRefactoryTestDataApp.
	self executeRefactoring: refactoring.
	self assert: ((refactoring model classNamed: #RBRefactoryTestDataApp) parseTreeFor: #inlineMethod) = (RBParser parseMethod: 'inlineMethod
										^self
													foo;
													inlineMethod;
													bar')
]

{ #category : #tests }
RBInlineTemporaryTest >> testInlineTemporaryBadInterval [
	self shouldFail: (RBInlineTemporaryRefactoring 
			inline: (self 
					convertInterval: (29 to: 100)
					for: (RBRefactoryTestDataApp sourceCodeAt: #moveDefinition))
			from: #moveDefinition
			in: RBRefactoryTestDataApp)
]

{ #category : #tests }
RBInlineTemporaryTest >> testInlineTemporaryMutlipleAssignment [
	self shouldFail: (RBInlineTemporaryRefactoring 
			inline: (self 
					convertInterval: (60 to: 83)
					for: (RBRefactoryTestDataApp sourceCodeAt: #moveDefinition))
			from: #moveDefinition
			in: RBRefactoryTestDataApp)
]

{ #category : #tests }
RBInlineTemporaryTest >> testInlineTemporaryReadBeforeWritten [
	self shouldFail: (RBInlineTemporaryRefactoring 
			inline: (self 
					convertInterval: (48 to: 56)
					for: (RBRefactoryTestDataApp sourceCodeAt: #inlineTemporary))
			from: #inlineTemporary
			in: RBRefactoryTestDataApp)
]
