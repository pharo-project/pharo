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

{ #category : 'execution' }
SycRemoveClassCommand >> execute [

	^ ((ReRemoveClassDriver newApplication: self application) scopes: toolContext refactoringScopes classes: classes) runRefactoring 
]

{ #category : 'accessing' }
SycRemoveClassCommand >> iconName [
	^#remove
]

{ #category : 'accessing' }
SycRemoveClassCommand >> name [
	^'(R) Remove'
]

{ #category : 'execution' }
SycRemoveClassCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.
	toolContext := aToolContext.
]
