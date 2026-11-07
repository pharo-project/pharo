"
I am a command to merge given variable into another
"
Class {
	#name : 'SycMergeVariableCommand',
	#superclass : 'SycRefactorVariableCommand',
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

{ #category : 'activation' }
SycMergeVariableCommand class >> fullBrowserShortcutActivation [
]

{ #category : 'accessing' }
SycMergeVariableCommand class >> iconName [
	^ #merge
]

{ #category : 'activation' }
SycMergeVariableCommand class >> sourceCodeMenuActivation [
]

{ #category : 'activation' }
SycMergeVariableCommand class >> sourceCodeShortcutActivation [
]

{ #category : 'execution' }
SycMergeVariableCommand >> applyCommandResult [
	super applyCommandResult.
	self context showVariableNamed: newName
]

{ #category : 'execution' }
SycMergeVariableCommand >> classRefactoring [
	^ RBMergeInstanceVariableIntoAnother
]

{ #category : 'execution' }
SycMergeVariableCommand >> executeRefactoring [

	| refactoring |
	refactoring := variable createVariableRefactoring: self classRefactoring.
	refactoring newName: self newVariableName.
	^ refactoring execute
]

{ #category : 'testing' }
SycMergeVariableCommand >> isAvailableInMenuOf: aToolContext [

	^ aToolContext isFullBrowserVariableContext
]

{ #category : 'accessing' }
SycMergeVariableCommand >> name [
	^'Merge into another'
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

{ #category : 'accessing' }
SycMergeVariableCommand >> order [

	^ 1.1
]

{ #category : 'execution' }
SycMergeVariableCommand >> prepareFullExecution [

	super prepareFullExecution.
	variable := self context lastSelectedVariable.
	newName := StBrowserSearchPresenter searchConfiguring: [ :presenter :dialog |
			           dialog title: 'Select a variable'.
			           presenter items: (self context browser itemsForQuery: ClyAllInstanceVariablesQuery sorted inScope: (ClyClassScope of: variable definingClass) withInheritedScope) ]
]

{ #category : 'accessing' }
SycMergeVariableCommand >> variable [
	^ variable
]

{ #category : 'accessing' }
SycMergeVariableCommand >> variable: anObject [
	variable := anObject
]
