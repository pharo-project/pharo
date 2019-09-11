"
I am a command to rename given class.

Internal Representation and Key Implementation Points.

    Instance Variables
	newName:		<String>
"
Class {
	#name : #SycRenameClassCommand,
	#superclass : #SycSingleClassCommand,
	#instVars : [
		'newName'
	],
	#category : #'SystemCommands-ClassCommands'
}

{ #category : #accessing }
SycRenameClassCommand >> defaultMenuIconName [ 
	^ #edit
]

{ #category : #accessing }
SycRenameClassCommand >> defaultMenuItemName [
	^'Rename'
]

{ #category : #execution }
SycRenameClassCommand >> execute [
	
	| refactoring |
	refactoring := RBRenameClassRefactoring
		rename: targetClass
		to: newName.
	refactoring execute
]

{ #category : #accessing }
SycRenameClassCommand >> newName [
	^ newName
]

{ #category : #accessing }
SycRenameClassCommand >> newName: anObject [
	newName := anObject
]

{ #category : #execution }
SycRenameClassCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.
	
	newName := UIManager default 
		request: 'New name of the class' initialAnswer: targetClass name title: 'Rename a class'.
		
	newName isEmptyOrNil | (newName = targetClass name) ifTrue: [ CmdCommandAborted signal]
]
