"
I am a submenu for rarely used refactorings
"
Class {
	#name : #SycMoreRefactorsMenuGroup,
	#superclass : #CmdMenuGroup,
	#category : #'SystemCommands-RefactoringSupport'
}

{ #category : #accessing }
SycMoreRefactorsMenuGroup >> buildContextMenu: aMenu [
	aMenu addLine.
	
	super buildContextMenu: aMenu.
	aMenu addLine.
]

{ #category : #accessing }
SycMoreRefactorsMenuGroup >> name [
	^ 'More refactorings'
]

{ #category : #accessing }
SycMoreRefactorsMenuGroup >> order [
	^10100
]
