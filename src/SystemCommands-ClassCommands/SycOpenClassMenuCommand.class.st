"
I am a command to open special class menu.
I show in menu all commands annotated by SycClassMenuActivation class
"
Class {
	#name : 'SycOpenClassMenuCommand',
	#superclass : 'SycOpenContextMenuCommand',
	#category : 'SystemCommands-ClassCommands',
	#package : 'SystemCommands-ClassCommands'
}

{ #category : 'execution' }
SycOpenClassMenuCommand >> activationStrategy [
	^ SycClassMenuActivation
]

{ #category : 'context menu' }
SycOpenClassMenuCommand >> cmCommandClass [
	^ SycClassCmCommand
]

{ #category : 'execution' }
SycOpenClassMenuCommand >> defaultMenuItemName [
	^'Refactorings'
]

{ #category : 'accessing' }
SycOpenClassMenuCommand >> description [

	^ 'Opens special class menu.'
]
