"
I am a command to rename a variable. 
"
Class {
	#name : 'SycRenameVariableCommand',
	#superclass : 'CmdCommand',
	#instVars : [
		'toolContext'
	],
	#category : 'SystemCommands-VariableCommands',
	#package : 'SystemCommands-VariableCommands'
}

{ #category : 'testing' }
SycRenameVariableCommand class >> canBeExecutedInContext: aToolContext [
	^aToolContext isVariableSelected and: [ aToolContext isGlobalVariableSelected not ]
]

{ #category : 'accessing' }
SycRenameVariableCommand class >> defaultMenuIconName [
	^ #edit
]

{ #category : 'accessing' }
SycRenameVariableCommand >> defaultMenuItemName [

	^ 'Rename'
]

{ #category : 'execution' }
SycRenameVariableCommand >> execute [

	| variable driver |
	variable := toolContext lastSelectedVariable.
	
	driver := variable isClassVariable
		          ifFalse: [ ReRenameInstanceVariableDriver new ]
		          ifTrue: [ ReRenameSharedVariableDriver new ].

	(driver
		 scopes: toolContext refactoringScopes
		 variable: variable name
		 for: variable definingClass) runRefactoring
]

{ #category : 'execution' }
SycRenameVariableCommand >> prepareFullExecutionInContext: aToolContext [

	super prepareFullExecutionInContext: aToolContext.
	toolContext := aToolContext
]
