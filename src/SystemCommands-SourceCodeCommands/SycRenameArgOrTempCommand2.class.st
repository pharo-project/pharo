"
I am a command to rename temp variable in given method.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	newName:		<String>
"
Class {
	#name : 'SycRenameArgOrTempCommand2',
	#superclass : 'SycSourceCodeRefactoringCommand',
	#instVars : [
		'newName',
		'toolContext'
	],
	#category : 'SystemCommands-SourceCodeCommands',
	#package : 'SystemCommands-SourceCodeCommands'
}

{ #category : 'testing' }
SycRenameArgOrTempCommand2 class >> canBeExecutedInContext: aSourceCodeContext [

	^ (super canBeExecutedInContext: aSourceCodeContext) and: [
		  aSourceCodeContext isArgOrTempVariableSelected ]
]

{ #category : 'execution' }
SycRenameArgOrTempCommand2 >> applyResultInContext: aSourceCodeContext [
	super applyResultInContext: aSourceCodeContext.

	aSourceCodeContext showVariableNamed: newName
]

{ #category : 'accessing' }
SycRenameArgOrTempCommand2 >> defaultMenuIconName [
	^ #edit
]

{ #category : 'accessing' }
SycRenameArgOrTempCommand2 >> defaultMenuItemName [
	^'(R) Rename temp2'
]

{ #category : 'accessing' }
SycRenameArgOrTempCommand2 >> description [

	^ 'Renames temp variable in given method.'
]

{ #category : 'execution' }
SycRenameArgOrTempCommand2 >> execute [
"self halt. "
	^ (ReRenameArgumentOrTemporaryDriver2 new
		   sourceNode: sourceNode;
		   method: method;
		   scopes: toolContext refactoringScopes;
		   yourself) runRefactoring
]

{ #category : 'execution' }
SycRenameArgOrTempCommand2 >> prepareFullExecutionInContext: aToolContext [

	super prepareFullExecutionInContext: aToolContext.

	toolContext := aToolContext
]
