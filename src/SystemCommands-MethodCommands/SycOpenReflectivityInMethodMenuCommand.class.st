"
I am a command to open special method menu containing metalink related commands.
I show in menu all commands annotated by SycReflectivityMenuActivation
"
Class {
	#name : #SycOpenReflectivityInMethodMenuCommand,
	#superclass : #SycOpenDebuggingInMethodMenuCommand,
	#category : #'SystemCommands-MethodCommands'
}

{ #category : #activation }
SycOpenReflectivityInMethodMenuCommand class >> methodContextMenuActivation [
	<classAnnotation>

	^CmdContextMenuActivation byRootGroupItemOrder: 1.5 for: ClyMethod asCalypsoItemContext
]

{ #category : #execution }
SycOpenReflectivityInMethodMenuCommand >> activationStrategy [
	^SycReflectivityMenuActivation
]

{ #category : #accessing }
SycOpenReflectivityInMethodMenuCommand >> defaultMenuIconName [
	^ #smallObjects
]

{ #category : #accessing }
SycOpenReflectivityInMethodMenuCommand >> defaultMenuItemName [

	^ 'Reflectivity'
]
