Class {
	#name : 'SycRenameVariableCommand2',
	#superclass : 'CmdCommand',
	#instVars : [
		'toolContext'
	],
	#category : 'SystemCommands-VariableCommands',
	#package : 'SystemCommands-VariableCommands'
}

{ #category : 'testing' }
SycRenameVariableCommand2 class >> canBeExecutedInContext: aToolContext [
	^aToolContext isVariableSelected and: [ aToolContext isGlobalVariableSelected not ]
]

{ #category : 'accessing' }
SycRenameVariableCommand2 class >> defaultMenuIconName [
	^ #edit
]

{ #category : 'accessing' }
SycRenameVariableCommand2 >> defaultMenuItemName [

	^ 'Rename'
]

{ #category : 'execution' }
SycRenameVariableCommand2 >> execute [

	| variable driver |
	variable := toolContext lastSelectedVariable.
	
	driver := variable isClassVariable
		          ifFalse: [ RBRenameInstanceVariableDriver new ]
		          ifTrue: [ RBRenameSharedVariableDriver new ].

	(driver
		 scopes: toolContext refactoringScopes
		 variable: variable name
		 for: variable definingClass) runRefactoring
]

{ #category : 'execution' }
SycRenameVariableCommand2 >> prepareFullExecutionInContext: aToolContext [

	super prepareFullExecutionInContext: aToolContext.
	toolContext := aToolContext
]
