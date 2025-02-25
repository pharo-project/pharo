"
I am a command to open the breakpoint menu after a right click on the source code.
I show in the menu all commands annotated by SycDebuggingMenuActivation
"
Class {
	#name : 'SycOpenDebuggingMenuCommand',
	#superclass : 'SycOpenContextMenuCommand',
	#category : 'SystemCommands-SourceCodeCommands',
	#package : 'SystemCommands-SourceCodeCommands'
}

{ #category : 'execution' }
SycOpenDebuggingMenuCommand >> activationStrategy [
	^SycDebuggingMenuActivation
]

{ #category : 'context menu' }
SycOpenDebuggingMenuCommand >> cmCommandClass [

	^ SycSourceCodeCmCommand
]

{ #category : 'accessing' }
SycOpenDebuggingMenuCommand >> defaultMenuIconName [
	^ #halt
]

{ #category : 'accessing' }
SycOpenDebuggingMenuCommand >> defaultMenuItemName [

	^ 'Breakpoints'
]

{ #category : 'accessing' }
SycOpenDebuggingMenuCommand >> description [

	^ 'Opens the breakpoint menu after a right click on the source code.'
]
