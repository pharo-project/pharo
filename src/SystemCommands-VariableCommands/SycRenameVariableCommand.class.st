"
I am a command to rename a variable. 
"
Class {
	#name : 'SycRenameVariableCommand',
	#superclass : 'ClyCommand',
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
SycRenameVariableCommand >> description [

	^ 'Rename variable.'
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

{ #category : 'accessing' }
SycRenameVariableCommand >> iconName [
	^ #edit
]

{ #category : 'accessing' }
SycRenameVariableCommand >> name [

	^ 'Rename'
]

{ #category : 'execution' }
SycRenameVariableCommand >> prepareFullExecutionInContext: aToolContext [

	super prepareFullExecutionInContext: aToolContext.
	toolContext := aToolContext
]
