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
	^'Rename2'
]

{ #category : 'execution' }
SycRenameVariableCommand2 >> execute [

	(RBRenameVariableDriver rename: toolContext lastSelectedVariable)
		scopes: toolContext refactoringScopes;
		model: (RBNamespace onEnvironment: RBBrowserEnvironment new);
		runRefactoring
]

{ #category : 'execution' }
SycRenameVariableCommand2 >> prepareFullExecutionInContext: aToolContext [

	super prepareFullExecutionInContext: aToolContext.
	toolContext := aToolContext
]
