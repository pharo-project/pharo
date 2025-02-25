"
I am a command to rename temp variable in given method.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	newName:		<String>
"
Class {
	#name : 'SycRenameArgOrTempCommand',
	#superclass : 'SycSourceCodeRefactoringCommand',
	#instVars : [
		'newName',
		'toolContext'
	],
	#category : 'SystemCommands-SourceCodeCommands',
	#package : 'SystemCommands-SourceCodeCommands'
}

{ #category : 'testing' }
SycRenameArgOrTempCommand class >> canBeExecutedInContext: aSourceCodeContext [

	^ (super canBeExecutedInContext: aSourceCodeContext) and: [
		  aSourceCodeContext isArgOrTempVariableSelected ]
]

{ #category : 'execution' }
SycRenameArgOrTempCommand >> applyResultInContext: aSourceCodeContext [
	super applyResultInContext: aSourceCodeContext.

	aSourceCodeContext showVariableNamed: newName
]

{ #category : 'accessing' }
SycRenameArgOrTempCommand >> defaultMenuIconName [
	^ #edit
]

{ #category : 'accessing' }
SycRenameArgOrTempCommand >> defaultMenuItemName [
	^'Rename temp'
]

{ #category : 'accessing' }
SycRenameArgOrTempCommand >> description [

	^ 'Renames temp variable in given method.'
]

{ #category : 'execution' }
SycRenameArgOrTempCommand >> execute [

	^ (ReRenameArgumentOrTemporaryDriver new
		   sourceNode: sourceNode;
		   method: method;
		   scopes: toolContext refactoringScopes;
		   yourself) runRefactoring
]

{ #category : 'testing' }
SycRenameArgOrTempCommand >> isComplexRefactoring [

	^ false
]

{ #category : 'execution' }
SycRenameArgOrTempCommand >> prepareFullExecutionInContext: aToolContext [

	super prepareFullExecutionInContext: aToolContext.

	toolContext := aToolContext
]
