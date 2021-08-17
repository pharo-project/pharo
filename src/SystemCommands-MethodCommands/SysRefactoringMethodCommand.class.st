Class {
	#name : #SysRefactoringMethodCommand,
	#superclass : #SycMethodCommand,
	#category : #'SystemCommands-MethodCommands'
}

{ #category : #execution }
SysRefactoringMethodCommand >> execute [

	self executeRefactorings
]

{ #category : #testing }
SysRefactoringMethodCommand >> isComplexRefactoring [
	^true
]

{ #category : #execution }
SysRefactoringMethodCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.
	self setUpModelFromContext: aToolContext.
]
