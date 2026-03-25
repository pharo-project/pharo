"
I am a command to move given instance variable to its class side
"
Class {
	#name : 'SycMoveInstanceVariableToClassSideCommand',
	#superclass : 'SycRefactorVariableCommand',
	#instVars : [
		'refactoringScopes'
	],
	#category : 'SystemCommands-VariableCommands',
	#package : 'SystemCommands-VariableCommands'
}

{ #category : 'accessing' }
SycMoveInstanceVariableToClassSideCommand >> defaultMenuIconName [
	^ #smallRedo
]

{ #category : 'accessing' }
SycMoveInstanceVariableToClassSideCommand >> defaultMenuItemName [

	^ '(T) Move to class side'
]

{ #category : 'execution' }
SycMoveInstanceVariableToClassSideCommand >> execute [

	^ ((ReMoveVariablesToClassSideDriver newApplication: self application) scopes: refactoringScopes variables: variables)
		  runRefactoring
]

{ #category : 'execution' }
SycMoveInstanceVariableToClassSideCommand >> prepareFullExecutionInContext: aToolContext [

	super prepareFullExecutionInContext: aToolContext.
	toolContext := aToolContext.
	refactoringScopes := toolContext refactoringScopes
]
