"
I am a command to push up given methods
"
Class {
	#name : #SycPushUpMethodCommand,
	#superclass : #SycMethodCommand,
	#category : #'SystemCommands-MethodCommands'
}

{ #category : #accessing }
SycPushUpMethodCommand >> defaultMenuItemName [
	^'Push up'
]

{ #category : #execution }
SycPushUpMethodCommand >> execute [

	(RBGenericInteractLayer method: methods) executePushUp 
]
