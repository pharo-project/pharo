"
I am a command to open the debugging menu after a right click on the source code.
I show in the menu all commands annotated by SycDebuggingMenuActivation
"
Class {
	#name : #SycOpenDebuggingMenuCommand,
	#superclass : #SycOpenSourceCodeMenuCommand,
	#category : #'SystemCommands-SourceCodeCommands'
}

{ #category : #execution }
SycOpenDebuggingMenuCommand >> activationStrategy [
	^SycDebuggingMenuActivation 
]

{ #category : #accessing }
SycOpenDebuggingMenuCommand >> defaultMenuIconName [
	^#smallDebugIcon 
]

{ #category : #accessing }
SycOpenDebuggingMenuCommand >> defaultMenuItemName [
	^'Debugging'
]
