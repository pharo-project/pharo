"
I am a command to move method to the class side of defining class
"
Class {
	#name : 'SycMoveMethodsToClassSideCommand',
	#superclass : 'SycMethodCommand',
	#instVars : [
		'refactoringScopes'
	],
	#category : 'SystemCommands-MethodCommands',
	#package : 'SystemCommands-MethodCommands'
}

{ #category : 'testing' }
SycMoveMethodsToClassSideCommand class >> canBeExecutedInContext: aToolContext [
	^(super canBeExecutedInContext: aToolContext)
		and: [ aToolContext isInstanceSideMethodSelected ]
]

{ #category : 'accessing' }
SycMoveMethodsToClassSideCommand >> defaultMenuIconName [
	^ #smallRedo
]

{ #category : 'accessing' }
SycMoveMethodsToClassSideCommand >> defaultMenuItemName [
	^'Move to class side'
]

{ #category : 'accessing' }
SycMoveMethodsToClassSideCommand >> description [

	^ 'Move method to the class side.'
]

{ #category : 'execution' }
SycMoveMethodsToClassSideCommand >> execute [

	(RBMoveMethodsToClassSideDriver scopes: refactoringScopes methods: methods) runRefactoring
]

{ #category : 'execution' }
SycMoveMethodsToClassSideCommand >> prepareFullExecutionInContext: aToolContext [

	super prepareFullExecutionInContext: aToolContext.
	refactoringScopes := aToolContext refactoringScopes
]
