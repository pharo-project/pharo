"
I am a command to open context menu of left side bar of text editor.
I use CmdTextLeftBarMenuActivation to collect all releated commands
"
Class {
	#name : #CmdOpenTextLeftBarMenuCommand,
	#superclass : #CmdOpenContextMenuCommand,
	#category : #'Commander-Activators-TextView'
}

{ #category : #activation }
CmdOpenTextLeftBarMenuCommand class >> contextClickActivation [
	<classAnnotation>
	
	^CmdTextLeftBarClickActivation byYellowButtonFor: CmdToolContext
	
]

{ #category : #execution }
CmdOpenTextLeftBarMenuCommand >> activationStrategy [
	^CmdTextLeftBarMenuActivation 
]
