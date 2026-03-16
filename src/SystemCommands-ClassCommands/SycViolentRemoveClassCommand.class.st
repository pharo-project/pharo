"
Violent Remove Class Refactoring: Force remove a class without performing safety checks. This may leave broken references in the system.
"
Class {
	#name : 'SycViolentRemoveClassCommand',
	#superclass : 'SycClassCommand',
	#instVars : [
		'toolContext'
	],
	#category : 'SystemCommands-ClassCommands',
	#package : 'SystemCommands-ClassCommands'
}

{ #category : 'accessing' }
SycViolentRemoveClassCommand >> defaultMenuIconName [
	^#remove
]

{ #category : 'accessing' }
SycViolentRemoveClassCommand >> defaultMenuItemName [
	^'(T) Violent Remove'
]

{ #category : 'execution' }
SycViolentRemoveClassCommand >> execute [

	^ ((ReViolentRemoveClassDriver newApplication: self application) scopes: toolContext refactoringScopes classes: classes) runRefactoring  
]

{ #category : 'execution' }
SycViolentRemoveClassCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.
	toolContext := aToolContext.
]
