"
I am a command to push up given methods
"
Class {
	#name : #SycPushUpMethodCommand,
	#superclass : #SycMethodCommand,
	#category : #'SystemCommands-MethodCommands'
}

{ #category : #accessing }
SycPushUpMethodCommand class >> defaultMenuIconName [
	^#up
]

{ #category : #activation }
SycPushUpMethodCommand class >> methodShortcutActivation [
	<classAnnotation>
	
	^CmdShortcutActivation pullUpFor: ClyMethod asCalypsoItemContext
]

{ #category : #accessing }
SycPushUpMethodCommand >> defaultMenuItemName [
	^'Push up'
]

{ #category : #execution }
SycPushUpMethodCommand >> execute [

	(RBInteractiveRefactoring method: methods) executePushUp 
]
