"
Renames temp variable in given method.
 
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

{ #category : 'execution' }
SycRenameArgOrTempCommand2 >> applyCommandResult [
	super applyCommandResult.
	newName ifNotNil: [ :name | self context showVariableNamed: name ]
]

{ #category : 'testing' }
SycRenameArgOrTempCommand2 >> canBeExecuted [

	^ super canBeExecuted and: [ self context isArgOrTempVariableSelected ]
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
