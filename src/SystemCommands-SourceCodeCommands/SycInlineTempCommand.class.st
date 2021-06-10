"
I am a command to inline selected temp variable (represented by source node)
"
Class {
	#name : #SycInlineTempCommand,
	#superclass : #SycSourceCodeRefactoringCommand,
	#category : #'SystemCommands-SourceCodeCommands'
}

{ #category : #execution }
SycInlineTempCommand class >> canBeExecutedInContext: aSourceCodeContext [
	super canBeExecutedInContext: aSourceCodeContext.
	
	^aSourceCodeContext isTempVariableSelected  
]

{ #category : #execution }
SycInlineTempCommand >> asRefactorings [
	| assignment |
	assignment := sourceNode methodNode assignmentNodes 
		detect: [:each | each variable = sourceNode ].

	^ { RBInlineTemporaryRefactoring	
		inline: assignment sourceInterval
		from: method selector
		in: method origin}
]

{ #category : #accessing }
SycInlineTempCommand >> defaultMenuIconName [ 
	^ ##smallRightFlushIcon
]

{ #category : #accessing }
SycInlineTempCommand >> defaultMenuItemName [
	^'Inline temp'
]
