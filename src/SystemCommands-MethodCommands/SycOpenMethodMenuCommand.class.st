"
I am a command to open special method menu.
I show in menu all commands annotated by SycMethodMenuActivation
"
Class {
	#name : 'SycOpenMethodMenuCommand',
	#superclass : 'SycOpenContextMenuCommand',
	#category : 'SystemCommands-MethodCommands',
	#package : 'SystemCommands-MethodCommands'
}

{ #category : 'execution' }
SycOpenMethodMenuCommand >> activationStrategy [
	^ SycMethodMenuActivation
]

{ #category : 'context menu' }
SycOpenMethodMenuCommand >> cmCommandClass [
	^ SycMethodCmCommand
]

{ #category : 'accessing' }
SycOpenMethodMenuCommand >> defaultMenuItemName [
	^'Refactorings'
]

{ #category : 'accessing' }
SycOpenMethodMenuCommand >> description [

	^ 'Opens special method menu.'
]
