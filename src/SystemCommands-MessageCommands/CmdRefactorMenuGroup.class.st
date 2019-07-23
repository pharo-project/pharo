"
I represent kind of reusable additional method group which users can reuse for grouping set of extra method refactoring commands.

"
Class {
	#name : #CmdRefactorMenuGroup,
	#superclass : #CmdMenuGroup,
	#category : #'SystemCommands-MessageCommands-Groups'
}

{ #category : #'as yet unclassified' }
CmdRefactorMenuGroup >> buildContextMenu: aMenu [
	aMenu addLine.
	
	super buildContextMenu: aMenu.
	aMenu addLine.
]

{ #category : #accessing }
CmdRefactorMenuGroup >> description [
	^'Refactoring commands'
]

{ #category : #accessing }
CmdRefactorMenuGroup >> name [
	^ 'Refactor'
]

{ #category : #accessing }
CmdRefactorMenuGroup >> order [
	^1
]
