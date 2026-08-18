"
Rename Class Refactoring: Change the name of a selected class, update all references, and preview changes before applying.

"
Class {
	#name : 'SycRenameClassCommand2',
	#superclass : 'SycSingleClassCommand',
	#instVars : [
		'toolContext'
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
	^'(R) Rename2'
]

{ #category : 'execution' }
SycRenameClassCommand2 >> execute [

	toolContext class = ClyFullBrowserClassContext
		ifTrue: [
			(ReRenameClassDriver2 rename: targetClass name)
				model: RBBrowserEnvironment new;
				scopes: toolContext refactoringScopes;
				runRefactoring ]
		ifFalse: [
			toolContext class = ClyClassDefinitionContext
				ifTrue: [
					(ReRenameClassDriver2 rename: toolContext selectedSourceNode value)
						model: RBBrowserEnvironment new;
						scopes: toolContext refactoringScopes;
						runRefactoring ]
				ifFalse: [ "we are in selection in a method "
					(ReRenameClassDriver2 rename:
							 toolContext selectedSourceNode value name)
						model: RBBrowserEnvironment new;
						scopes: toolContext refactoringScopes;
						runRefactoring ] ]
]

{ #category : 'execution' }
SycRenameClassCommand2 >> prepareFullExecutionInContext: aToolContext [
	
	super prepareFullExecutionInContext: aToolContext.
	toolContext := aToolContext

]
