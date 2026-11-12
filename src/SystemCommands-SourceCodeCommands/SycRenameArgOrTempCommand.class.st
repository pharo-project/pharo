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
		'newName'
	],
	#category : 'SystemCommands-SourceCodeCommands',
	#package : 'SystemCommands-SourceCodeCommands'
}

{ #category : 'execution' }
SycRenameArgOrTempCommand >> applyCommandResult [
	super applyCommandResult.

	self context showVariableNamed: newName
]

{ #category : 'testing' }
SycRenameArgOrTempCommand >> canBeExecuted [

	^ super canBeExecuted and: [ self context isArgOrTempVariableSelected ]
]

{ #category : 'accessing' }
SycRenameArgOrTempCommand >> description [

	^ 'Renames temp variable in given method.'
]

{ #category : 'execution' }
SycRenameArgOrTempCommand >> executeRefactoring [

	^ (ReRenameArgumentOrTemporaryDriver new
		   sourceNode: self sourceNode;
		   method: self method;
		   scopes: refactoringScopes;
		   yourself) runRefactoring
]

{ #category : 'accessing' }
SycRenameArgOrTempCommand >> iconName [
	^ #edit
]

{ #category : 'accessing' }
SycRenameArgOrTempCommand >> name [
	^' (Old refactor) rename temp'
]

{ #category : 'accessing' }
SycRenameArgOrTempCommand >> order [

	^ 1.2
]
