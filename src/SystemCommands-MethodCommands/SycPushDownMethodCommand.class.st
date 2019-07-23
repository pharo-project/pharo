"
I am a command to push down given methods
"
Class {
	#name : #SycPushDownMethodCommand,
	#superclass : #SycMethodCommand,
	#category : #'SystemCommands-MethodCommands'
}

{ #category : #accessing }
SycPushDownMethodCommand class >> defaultMenuIconName [
	^#down
]

{ #category : #activation }
SycPushDownMethodCommand class >> methodContextMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation 
		byItemOf: CmdRefactorMenuGroup 
		order: 31 
		for: ClyMethod asCalypsoItemContext
]

{ #category : #activation }
SycPushDownMethodCommand class >> methodShortcutActivation [
	<classAnnotation>
	
	^CmdShortcutActivation pushDownFor: ClyMethod asCalypsoItemContext
]

{ #category : #accessing }
SycPushDownMethodCommand >> defaultMenuItemName [
	^'Push down'
]

{ #category : #execution }
SycPushDownMethodCommand >> execute [

	(RBGenericInteractLayer method: methods) executePushDown 
]
