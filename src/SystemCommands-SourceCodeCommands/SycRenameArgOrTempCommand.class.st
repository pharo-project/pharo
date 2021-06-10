"
I am a command to rename temp variable in given method.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	newName:		<String>
"
Class {
	#name : #SycRenameArgOrTempCommand,
	#superclass : #SycSourceCodeRefactoringCommand,
	#instVars : [
		'newName'
	],
	#category : #'SystemCommands-SourceCodeCommands'
}

{ #category : #execution }
SycRenameArgOrTempCommand class >> canBeExecutedInContext: aSourceCodeContext [
	super canBeExecutedInContext: aSourceCodeContext.
	
	^ aSourceCodeContext isArgOrTempVariableSelected 
]

{ #category : #execution }
SycRenameArgOrTempCommand >> applyResultInContext: aSourceCodeContext [
	super applyResultInContext: aSourceCodeContext.
	
	aSourceCodeContext showVariableNamed: newName
]

{ #category : #execution }
SycRenameArgOrTempCommand >> asRefactorings [ 
	^ {RBRenameArgumentOrTemporaryRefactoring	
		renameTemporaryFrom: sourceNode sourceInterval
		to: newName
		in: method origin
		selector: method selector}
]

{ #category : #accessing }
SycRenameArgOrTempCommand >> defaultMenuIconName [ 
	^ #edit
]

{ #category : #accessing }
SycRenameArgOrTempCommand >> defaultMenuItemName [
	^'Rename temp'
]

{ #category : #execution }
SycRenameArgOrTempCommand >> prepareFullExecutionInContext: aSourceCodeContext [
	super prepareFullExecutionInContext: aSourceCodeContext.
	
	newName := UIManager default 
		request: 'New name of the variable' 
		initialAnswer: sourceNode name
		title: 'Rename a temp variable'.
	newName isEmptyOrNil | (newName = sourceNode name) ifTrue: [ CmdCommandAborted signal ]
]
