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

{ #category : 'context menu' }
SycOpenMethodMenuCommand >> cmCommandClasses [

	^ {
		  SycMethodCmCommand.
		  SycMessageCmCommand }
]

{ #category : 'accessing' }
SycOpenMethodMenuCommand >> description [

	^ 'Opens special method menu.'
]

{ #category : 'accessing' }
SycOpenMethodMenuCommand >> name [
	^'Refactorings'
]

{ #category : 'menu' }
SycOpenMethodMenuCommand >> pureMenuCommands [
	"The sorted commands of the method menu, placed in front of the activations."
	^ (ClyCommandGroupBuilder buildSortedForAll: self cmCommandClasses context: self context) commands
]

{ #category : 'menu' }
SycOpenMethodMenuCommand >> pureMenuPlacement [
	^ #prepend
]
