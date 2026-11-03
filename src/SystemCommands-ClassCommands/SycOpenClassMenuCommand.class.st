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

{ #category : 'execution' }
SycOpenClassMenuCommand >> addClyCommandsToMenu: aMenuMorph [

	| group |
	group := ClyCommandGroupBuilder
		         buildFor: self cmCommandClass
		         context: self context.
	^ ClyCommandGroupMenuRenderer
		  render: group
		  on: aMenuMorph
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
