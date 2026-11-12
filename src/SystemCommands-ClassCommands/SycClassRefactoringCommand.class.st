"
I am the root of the refactorings of the selected class, such as make abstract, duplicate, generate methods or insert a subclass or a superclass.
I add the refactoring scopes of the context to SycSingleClassCommand.
"
Class {
	#name : 'SycClassRefactoringCommand',
	#superclass : 'SycSingleClassCommand',
	#instVars : [
		'refactoringScopes'
	],
	#category : 'SystemCommands-ClassCommands',
	#package : 'SystemCommands-ClassCommands'
}

{ #category : 'preparation' }
SycClassRefactoringCommand >> prepareFullExecution [

	super prepareFullExecution.
	refactoringScopes := self context refactoringScopes
]
