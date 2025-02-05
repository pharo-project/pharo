"
Remove Class Refactoring: Safely delete a selected class, ensuring no references are left behind, and preview changes before applying.
"
Class {
	#name : 'SycRemoveClassCommand',
	#superclass : 'SycClassCommand',
	#instVars : [
		'toolContext'
	],
	#category : 'SystemCommands-ClassCommands',
	#package : 'SystemCommands-ClassCommands'
}

{ #category : 'accessing' }
SycRemoveClassCommand >> defaultMenuIconName [
	^#remove
]

{ #category : 'accessing' }
SycRemoveClassCommand >> defaultMenuItemName [
	^'(R) Remove'
]

{ #category : 'execution' }
SycRemoveClassCommand >> execute [

	(ReRemoveClassDriver new
				 scopes: toolContext refactoringScopes
				 classes:  classes) runRefactoring
]

{ #category : 'execution' }
SycRemoveClassCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.
	toolContext := aToolContext.
]
