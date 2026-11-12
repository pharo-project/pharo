"
Rename variable.
"
Class {
	#name : 'SycRenameVariableCommand',
	#superclass : 'SycRefactorVariableCommand',
	#category : 'SystemCommands-VariableCommands',
	#package : 'SystemCommands-VariableCommands'
}

{ #category : 'accessing' }
SycRenameVariableCommand class >> iconName [
	^ #edit
]

{ #category : 'testing' }
SycRenameVariableCommand >> canBeExecuted [

	^ self context isVariableSelected and: [ self context isGlobalVariableSelected not ]
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
