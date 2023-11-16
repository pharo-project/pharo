Class {
	#name : 'SycRenameClassCommand2',
	#superclass : 'SycSingleClassCommand',
	#instVars : [
		'newName',
		'toolContext',
		'model'
	],
	#category : 'SystemCommands-ClassCommands',
	#package : 'SystemCommands-ClassCommands'
}

{ #category : 'accessing' }
SycRenameClassCommand2 >> defaultMenuIconName [
	^ #edit
]

{ #category : 'accessing' }
SycRenameClassCommand2 >> defaultMenuItemName [
	^'Rename'
]

{ #category : 'execution' }
SycRenameClassCommand2 >> execute [
	"Pay attention when you specify a model that is a selection of the full environment. 
	Indeed imagine that you have a model of just one package, 
	the precondition may say that the new name is not the one of an already existing global 
	that is defined outside of your model (think of OrderedCollection) in this case
	when the refactoring will check then it will say that the name is ok, and you will destroy the system
	by creating a new class with the same name as an existing one.
	
	So this is we pass the model of the full environment and not a scope selected by the user."
	
	
	
	(RBRenameClassDriver
		rename: targetClass name) 
		model: RBBrowserEnvironment new;
		scopes: toolContext refactoringScopes; runRefactoring
]

{ #category : 'execution' }
SycRenameClassCommand2 >> prepareFullExecutionInContext: aToolContext [
	
	super prepareFullExecutionInContext: aToolContext.
	toolContext := aToolContext

]