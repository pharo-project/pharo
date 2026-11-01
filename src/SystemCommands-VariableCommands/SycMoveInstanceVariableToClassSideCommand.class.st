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

{ #category : 'execution' }
SycMoveInstanceVariableToClassSideCommand >> execute [

	^ ((ReMoveVariablesToClassSideDriver newApplication: self application) scopes: refactoringScopes variables: variables)
		  runRefactoring
]

{ #category : 'accessing' }
SycMoveInstanceVariableToClassSideCommand >> iconName [
	^ #smallRedo
]

{ #category : 'accessing' }
SycMoveInstanceVariableToClassSideCommand >> name [

	^ '(T) Move to class side'
]

{ #category : 'execution' }
SycMoveInstanceVariableToClassSideCommand >> prepareFullExecutionInContext: aToolContext [

	super prepareFullExecutionInContext: aToolContext.
	toolContext := aToolContext.

]
