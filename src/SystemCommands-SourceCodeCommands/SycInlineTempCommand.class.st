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
SycInlineTempCommand >> defaultMenuIconName [ 
	^ ##smallRightFlushIcon
]

{ #category : #accessing }
SycInlineTempCommand >> defaultMenuItemName [
	^'Inline temp'
]

{ #category : #execution }
SycInlineTempCommand >> execute [
	| assignment refactoring |
	assignment := sourceNode methodNode assignmentNodes 
		detect: [:each | each variable = sourceNode ].

	refactoring := RBInlineTemporaryRefactoring	
		inline: assignment sourceInterval
		from: method selector
		in: method origin.

	self executeRefactoring: refactoring.
]
