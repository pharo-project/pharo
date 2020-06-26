"
I am a command to rename temp variable in given method.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	newName:		<String>
"
Class {
	#name : #SycRenameLocalVariableCommand,
	#superclass : #SycSourceCodeCommand,
	#instVars : [
		'newName'
	],
	#category : #'SystemCommands-SourceCodeCommands'
}

{ #category : #execution }
SycRenameLocalVariableCommand class >> canBeExecutedInContext: aSourceCodeContext [
	super canBeExecutedInContext: aSourceCodeContext.
	
	^ aSourceCodeContext isLocalVariableSelected 
]

{ #category : #execution }
SycRenameLocalVariableCommand >> applyResultInContext: aSourceCodeContext [
	super applyResultInContext: aSourceCodeContext.
	
	aSourceCodeContext showVariableNamed: newName
]

{ #category : #accessing }
SycRenameLocalVariableCommand >> defaultMenuIconName [ 
	^ #edit
]

{ #category : #accessing }
SycRenameLocalVariableCommand >> defaultMenuItemName [
	^'Rename local'
]

{ #category : #execution }
SycRenameLocalVariableCommand >> execute [
	| refactoring |
	refactoring := RBRenameArgumentOrTemporaryRefactoring	
		renameTemporaryFrom: sourceNode sourceInterval
		to: newName
		in: method origin
		selector: method selector.
	
	self executeRefactoring: refactoring.
]

{ #category : #execution }
SycRenameLocalVariableCommand >> prepareFullExecutionInContext: aSourceCodeContext [
	super prepareFullExecutionInContext: aSourceCodeContext.
	
	newName := UIManager default 
		request: 'New name of the variable' 
		initialAnswer: sourceNode name
		title: 'Rename a temp variable'.
	newName isEmptyOrNil | (newName = sourceNode name) ifTrue: [ CmdCommandAborted signal ]
]
