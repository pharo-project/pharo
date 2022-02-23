"
I am a command to open special method menu containing debugging commands.
I show in menu all commands annotated by SycAdvancedDebuggingMenuActivation
"
Class {
	#name : #SycOpenAdvancedDebuggingInMethodMenuCommand,
	#superclass : #SycOpenMethodMenuCommand,
	#category : #'SystemCommands-MethodCommands'
}

{ #category : #activation }
SycOpenAdvancedDebuggingInMethodMenuCommand class >> methodContextMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation byRootGroupItemOrder: 1.5 for: ClyMethod asCalypsoItemContext
]

{ #category : #execution }
SycOpenAdvancedDebuggingInMethodMenuCommand >> activationStrategy [
	^SycAdvancedDebuggingMenuActivation
]

{ #category : #accessing }
SycOpenAdvancedDebuggingInMethodMenuCommand >> defaultMenuIconName [
	^ #smallDebug
]

{ #category : #accessing }
SycOpenAdvancedDebuggingInMethodMenuCommand >> defaultMenuItemName [
	^'Debugging'
]
