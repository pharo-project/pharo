"
I am a command to open special method menu containing breakpoint commands.
I show in menu all commands annotated by SycDebuggingMenuActivation
"
Class {
	#name : #SycOpenDebuggingInMethodMenuCommand,
	#superclass : #SycOpenMethodMenuCommand,
	#category : #'SystemCommands-MethodCommands'
}

{ #category : #activation }
SycOpenDebuggingInMethodMenuCommand class >> methodContextMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation byRootGroupItemOrder: 1.4 for: ClyMethod asCalypsoItemContext
]

{ #category : #execution }
SycOpenDebuggingInMethodMenuCommand >> activationStrategy [

	^ SycDebuggingMenuActivation
]

{ #category : #'context menu' }
SycOpenDebuggingInMethodMenuCommand >> cmCommandClass [

	^ SycDebuggingInMethodCmCommand
]

{ #category : #accessing }
SycOpenDebuggingInMethodMenuCommand >> defaultMenuIconName [
	^ #halt
]

{ #category : #accessing }
SycOpenDebuggingInMethodMenuCommand >> defaultMenuItemName [
	^'Breakpoints'
]
