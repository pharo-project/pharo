"
I am a command to inline selected temp variable (represented by source node)
"
Class {
	#name : #SycInlineTempCommand,
	#superclass : #SycSourceCodeCommand,
	#category : #'SystemCommands-SourceCodeCommands'
}

{ #category : #execution }
SycInlineTempCommand class >> canBeExecutedInContext: aSourceCodeContext [
	super canBeExecutedInContext: aSourceCodeContext.
	
	^aSourceCodeContext isTempVariableSelected  
]

{ #category : #accessing }
SycInlineTempCommand >> defaultMenuItemName [
	^'Inline temp'
]

{ #category : #execution }
SycInlineTempCommand >> execute [
	(RBGenericInteractLayer sourceNode: sourceNode method: method) executeInlineTemp
]
