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

{ #category : 'testing' }
SycRefactoringMethodCommand >> isComplexRefactoring [
	^true
]

{ #category : 'execution' }
SycRefactoringMethodCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.
	self setUpModelFromContext: aToolContext
]
