Class {
	#name : 'SycRefactoringMethodCommand',
	#superclass : 'SycMethodCommand',
	#category : 'SystemCommands-MethodCommands',
	#package : 'SystemCommands-MethodCommands'
}

{ #category : 'execution' }
SycRefactoringMethodCommand >> execute [

	self executeRefactorings
]

{ #category : 'execution' }
SycRefactoringMethodCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.
	self setUpModelFromContext: aToolContext
]
