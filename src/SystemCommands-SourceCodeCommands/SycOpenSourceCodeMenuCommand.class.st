"
I am a command to open special source code menu.
I show in menu all commands annotated by SycSourceCodeMenuActivation
"
Class {
	#name : 'SycOpenSourceCodeMenuCommand',
	#superclass : 'SycOpenContextMenuCommand',
	#category : 'SystemCommands-SourceCodeCommands',
	#package : 'SystemCommands-SourceCodeCommands'
}

{ #category : 'execution' }
SycOpenSourceCodeMenuCommand >> activationStrategy [
	^SycSourceCodeMenuActivation
]

{ #category : 'context menu' }
SycOpenSourceCodeMenuCommand >> cmCommandClass [

	^ SycSourceCodeCmCommand
]

{ #category : 'accessing' }
SycOpenSourceCodeMenuCommand >> defaultMenuIconName [
	^#smallAuthoringTools
]

{ #category : 'accessing' }
SycOpenSourceCodeMenuCommand >> defaultMenuItemName [
	^'Source code'
]

{ #category : 'accessing' }
SycOpenSourceCodeMenuCommand >> description [

	^ 'Opens special source code menu.'
]

{ #category : 'execution' }
SycOpenSourceCodeMenuCommand >> execute [

	context selectedTextInterval ifEmpty: [
		context showSourceNode].

	super execute
]
