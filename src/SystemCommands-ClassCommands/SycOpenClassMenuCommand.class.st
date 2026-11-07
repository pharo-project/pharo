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

{ #category : 'accessing' }
SycOpenClassMenuCommand >> description [

	^ 'Opens special class menu.'
]

{ #category : 'execution' }
SycOpenClassMenuCommand >> name [
	^'Refactorings'
]

{ #category : 'menu' }
SycOpenClassMenuCommand >> pureMenuCommands [
	"The commands of the class of this opener that apply here, in the order they are built."
	^ self classFamilyPureMenuCommands
]
