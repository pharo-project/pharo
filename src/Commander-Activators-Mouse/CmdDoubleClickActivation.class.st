"
I represent activation of commands using mouse double click event
"
Class {
	#name : #CmdDoubleClickActivation,
	#superclass : #CmdMouseCommandActivation,
	#category : #'Commander-Activators-Mouse'
}

{ #category : #'morphs setup' }
CmdDoubleClickActivation class >> morphicEventType [
	^#doubleClick
]
