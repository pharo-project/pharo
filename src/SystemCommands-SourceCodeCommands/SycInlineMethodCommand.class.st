"
I am a command to inline ""self send"" method directly into sender method
"
Class {
	#name : #SycInlineMethodCommand,
	#superclass : #SycSourceCodeRefactoringCommand,
	#category : #'SystemCommands-SourceCodeCommands'
}

{ #category : #execution }
SycInlineMethodCommand class >> canBeExecutedInContext: aSourceCodeContext [
	super canBeExecutedInContext: aSourceCodeContext.
	
	^aSourceCodeContext isMethodSelected not and: [ aSourceCodeContext isMessageSelected ]
]

{ #category : #execution }
SycInlineMethodCommand >> asRefactorings [

	^ {RBInlineMethodRefactoring
		  inline: sourceNode sourceInterval
		  inMethod: method selector
		  forClass: method origin}
]

{ #category : #accessing }
SycInlineMethodCommand >> defaultMenuIconName [ 
	^ #smallRightFlushIcon
]

{ #category : #accessing }
SycInlineMethodCommand >> defaultMenuItemName [
	^ 'Inline method'
]
