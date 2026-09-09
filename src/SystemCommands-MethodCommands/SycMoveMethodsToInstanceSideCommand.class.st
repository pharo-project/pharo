"
I am a command to move method to the instance side of defining class
"
Class {
	#name : 'SycMoveMethodsToInstanceSideCommand',
	#superclass : 'SycMethodCommand',
	#instVars : [
		'refactoringScopes'
	],
	#category : 'SystemCommands-MethodCommands',
	#package : 'SystemCommands-MethodCommands'
}

{ #category : 'testing' }
SycMoveMethodsToInstanceSideCommand class >> canBeExecutedInContext: aToolContext [
	^(super canBeExecutedInContext: aToolContext)
		and: [ aToolContext isClassSideMethodSelected ]
]

{ #category : 'accessing' }
SycMoveMethodsToInstanceSideCommand >> defaultMenuIconName [
	^ #smallRedo
]

{ #category : 'accessing' }
SycMoveMethodsToInstanceSideCommand >> defaultMenuItemName [
	^'Move to instance side'
]

{ #category : 'accessing' }
SycMoveMethodsToInstanceSideCommand >> description [

	^ 'Move method to the instance side.'
]

{ #category : 'execution' }
SycMoveMethodsToInstanceSideCommand >> execute [

	methods
		collect: [ :each |
				(ReMoveMethodToClassDriver newApplication: self application)
					scopes: refactoringScopes
					method: each
					class: each origin instanceSide ]
		thenDo: [ :each | each runRefactoring ]
]

{ #category : 'execution' }
SycMoveMethodsToInstanceSideCommand >> prepareFullExecutionInContext: aToolContext [

	super prepareFullExecutionInContext: aToolContext.
	refactoringScopes := aToolContext refactoringScopes
]
