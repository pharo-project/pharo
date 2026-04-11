"
I am a command to move given instance variable to its class side
"
Class {
	#name : 'SycMoveInstanceVariableToClassSideCommand',
	#superclass : 'SycRefactorVariableCommand',
	#category : 'SystemCommands-VariableCommands',
	#package : 'SystemCommands-VariableCommands'
}

{ #category : 'activation' }
SycMoveInstanceVariableToClassSideCommand class >> fullBrowserMenuActivation [

	<classAnnotation>
	^ CmdContextMenuActivation byRootGroupItemFor: ClyFullBrowserVariableContext
]

{ #category : 'activation' }
SycMoveInstanceVariableToClassSideCommand class >> sourceCodeMenuActivation [
	<classAnnotation>

	^SycSourceCodeMenuActivation byRootGroupItemOrder: 1.6 for: ClySourceCodeContext
]

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

]
