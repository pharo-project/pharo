"
I am a command to rename temp variable in given method.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	newName:		<String>
"
Class {
	#name : #SycRenameTempCommand,
	#superclass : #SycSourceCodeCommand,
	#instVars : [
		'newName'
	],
	#category : #'SystemCommands-SourceCodeCommands'
}

{ #category : #execution }
SycRenameTempCommand class >> canBeExecutedInContext: aSourceCodeContext [
	super canBeExecutedInContext: aSourceCodeContext.
	
	^aSourceCodeContext isTempVariableSelected 
]

{ #category : #accessing }
SycRenameTempCommand >> defaultMenuItemName [
	^'Rename temp'
]

{ #category : #execution }
SycRenameTempCommand >> execute [
	(RBInteractiveRenameTemp sourceNode: sourceNode method: method) executeRenameTemp
]
