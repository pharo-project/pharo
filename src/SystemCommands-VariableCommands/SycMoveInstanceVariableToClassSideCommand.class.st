"
I am a command to move given instance variable to its class side
"
Class {
	#name : 'SycMoveInstanceVariableToClassSideCommand',
	#superclass : 'SycRefactorVariableCommand',
	#category : 'SystemCommands-VariableCommands',
	#package : 'SystemCommands-VariableCommands'
}

{ #category : 'execution' }
SycMoveInstanceVariableToClassSideCommand >> executeRefactoring [

	^ ((ReMoveVariablesToClassSideDriver newApplication: self application) scopes: refactoringScopes variables: variables)
		runRefactoring
]

{ #category : 'accessing' }
SycMoveInstanceVariableToClassSideCommand >> fullBrowserOrder [

	^ 1000
]

{ #category : 'accessing' }
SycMoveInstanceVariableToClassSideCommand >> icon [

	^ self iconNamed: #smallRedo
]

{ #category : 'accessing' }
SycMoveInstanceVariableToClassSideCommand >> name [

	^ '(T) Move to class side'
]

{ #category : 'accessing' }
SycMoveInstanceVariableToClassSideCommand >> order [

	^ 1.6
]
