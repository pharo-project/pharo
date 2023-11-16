Class {
	#name : 'SycRenameClassCommand2',
	#superclass : 'SycSingleClassCommand',
	#instVars : [
		'newName',
		'toolContext'
	],
	#category : 'SystemCommands-ClassCommands',
	#package : 'SystemCommands-ClassCommands'
}

{ #category : 'accessing' }
SycRenameClassCommand2 >> defaultMenuIconName [
	^ #edit
]

{ #category : 'accessing' }
SycRenameClassCommand2 >> defaultMenuItemName [
	^'Rename 2(WithDriver)'
]

{ #category : 'execution' }
SycRenameClassCommand2 >> execute [

	(RBRenameClassDriver
		rename: targetClass name
		to: newName) scopes: toolContext refactoringScopes; runRefactoring
]

{ #category : 'accessing' }
SycRenameClassCommand2 >> newName [
	^ newName
]

{ #category : 'accessing' }
SycRenameClassCommand2 >> newName: anObject [
	newName := anObject
]

{ #category : 'execution' }
SycRenameClassCommand2 >> prepareFullExecutionInContext: aToolContext [

	super prepareFullExecutionInContext: aToolContext.
	toolContext := aToolContext

]
