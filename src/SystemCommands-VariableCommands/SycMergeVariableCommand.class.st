"
I am a command to merge given variable into another
"
Class {
	#name : 'SycMergeVariableCommand',
	#superclass : 'CmdCommand',
	#instVars : [
		'variable',
		'newName'
	],
	#category : 'SystemCommands-VariableCommands',
	#package : 'SystemCommands-VariableCommands'
}

{ #category : 'testing' }
SycMergeVariableCommand class >> canBeExecutedInContext: aToolContext [
	^aToolContext isVariableSelected and: [ aToolContext isGlobalVariableSelected not ]
]

{ #category : 'accessing' }
SycMergeVariableCommand class >> defaultMenuIconName [
	^ #merge
]

{ #category : 'activation' }
SycMergeVariableCommand class >> fullBrowserShortcutActivation [
]

{ #category : 'activation' }
SycMergeVariableCommand class >> sourceCodeMenuActivation [
]

{ #category : 'activation' }
SycMergeVariableCommand class >> sourceCodeShortcutActivation [
]

{ #category : 'execution' }
SycMergeVariableCommand >> applyResultInContext: aToolContext [
	super applyResultInContext: aToolContext.
	aToolContext showVariableNamed: newName
]

{ #category : 'execution' }
SycMergeVariableCommand >> classRefactoring [
	^ RBMergeInstanceVariableIntoAnother
]

{ #category : 'accessing' }
SycMergeVariableCommand >> defaultMenuItemName [
	^'Merge into another'
]

{ #category : 'execution' }
SycMergeVariableCommand >> execute [

	| refactoring |
	refactoring := variable createVariableRefactoring: self classRefactoring.
	refactoring newName: self newVariableName.
	refactoring execute
]

{ #category : 'accessing' }
SycMergeVariableCommand >> newName [
	^ newName
]

{ #category : 'accessing' }
SycMergeVariableCommand >> newName: anObject [
	newName := anObject
]

{ #category : 'execution' }
SycMergeVariableCommand >> newVariableName [

	^ newName name
]

{ #category : 'execution' }
SycMergeVariableCommand >> prepareFullExecutionInContext: aToolContext [

	super prepareFullExecutionInContext: aToolContext.
	variable := aToolContext lastSelectedVariable.
	self requestNewNameIn: aToolContext
]

{ #category : 'execution' }
SycMergeVariableCommand >> requestNewNameIn: aToolContext [
	newName := aToolContext
		           requestSimpleVariable: 'Select a variable'
		           from: (Array with: variable definingClass)
]

{ #category : 'accessing' }
SycMergeVariableCommand >> variable [
	^ variable
]

{ #category : 'accessing' }
SycMergeVariableCommand >> variable: anObject [
	variable := anObject
]
