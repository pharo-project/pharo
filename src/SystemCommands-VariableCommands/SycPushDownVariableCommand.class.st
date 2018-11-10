"
I am a command to push down given variables
"
Class {
	#name : #SycPushDownVariableCommand,
	#superclass : #SycRefactorVariableCommand,
	#category : #'SystemCommands-VariableCommands'
}

{ #category : #execution }
SycPushDownVariableCommand >> asRefactorings [

	^self 
		createRefactorings: RBPushDownInstanceVariableRefactoring
]

{ #category : #accessing }
SycPushDownVariableCommand >> defaultMenuItemName [
	^'Push down'
]
