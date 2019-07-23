"
I am a command to inline ""self send"" method directly into sender method
"
Class {
	#name : #SycInlineMethodCommand,
	#superclass : #SycSourceCodeCommand,
	#category : #'SystemCommands-SourceCodeCommands'
}

{ #category : #execution }
SycInlineMethodCommand class >> canBeExecutedInContext: aSourceCodeContext [
	super canBeExecutedInContext: aSourceCodeContext.
	
	^aSourceCodeContext isMethodSelected not and: [ aSourceCodeContext isMessageSelected ]
]

{ #category : #accessing }
SycInlineMethodCommand >> defaultMenuItemName [
	^ 'Inline method'
]

{ #category : #execution }
SycInlineMethodCommand >> execute [
	(RBGenericInteractLayer sourceNode: sourceNode method: method) executeInlineMethod
]
