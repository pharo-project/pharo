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
		'newName'
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
SycRenameArgOrTempCommand2 >> applyCommandResult [
	super applyCommandResult.
	newName ifNotNil: [ :name | self context showVariableNamed: name ]
]

{ #category : 'accessing' }
SycRenameArgOrTempCommand2 >> description [

	^ 'Renames temp variable in given method.'
]

{ #category : 'execution' }
SycRenameArgOrTempCommand2 >> executeRefactoring [
	"The driver asks for the new name and applies the rename; the name it ends up with is kept for the selection afterwards."
	| driver result |
	driver := ReRenameArgumentOrTemporaryDriver2 new
		sourceNode: self sourceNode;
		method: self method;
		scopes: refactoringScopes;
		yourself.
	result := driver runRefactoring.
	newName := driver newName.
	^ result
]

{ #category : 'accessing' }
SycRenameArgOrTempCommand2 >> iconName [
	^ #edit
]

{ #category : 'accessing' }
SycRenameArgOrTempCommand2 >> name [
	^'(R) Rename temp'
]

{ #category : 'accessing' }
SycRenameArgOrTempCommand2 >> order [

	^ 1.2
]
