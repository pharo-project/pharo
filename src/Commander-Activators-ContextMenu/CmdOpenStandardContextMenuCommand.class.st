"
I am a command to open standard context menu.

I use CmdContextMenuCommandActivation to collect menu items.
"
Class {
	#name : #CmdOpenStandardContextMenuCommand,
	#superclass : #CmdOpenContextMenuCommand,
	#category : #'Commander-Activators-ContextMenu'
}

{ #category : #execution }
CmdOpenStandardContextMenuCommand >> activationStrategy [
	^CmdContextMenuActivation 
]
