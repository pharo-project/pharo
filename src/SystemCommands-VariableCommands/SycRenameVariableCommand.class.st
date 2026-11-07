"
I am a command to rename a variable. 
"
Class {
	#name : 'SycRenameVariableCommand',
	#superclass : 'SycRefactorVariableCommand',
	#category : 'SystemCommands-VariableCommands',
	#package : 'SystemCommands-VariableCommands'
}

{ #category : 'testing' }
SycRenameVariableCommand class >> canBeExecutedInContext: aToolContext [
	^aToolContext isVariableSelected and: [ aToolContext isGlobalVariableSelected not ]
]

{ #category : 'accessing' }
SycRenameVariableCommand class >> iconName [
	^ #edit
]

{ #category : 'accessing' }
SycRenameVariableCommand >> description [

	^ 'Rename variable.'
]

{ #category : 'execution' }
SycRenameVariableCommand >> executeRefactoring [

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
SycRenameVariableCommand >> fullBrowserOrder [

	^ 1
]

{ #category : 'accessing' }
SycRenameVariableCommand >> name [

	^ 'Rename'
]

{ #category : 'accessing' }
SycRenameVariableCommand >> order [

	^ 1.1
]
