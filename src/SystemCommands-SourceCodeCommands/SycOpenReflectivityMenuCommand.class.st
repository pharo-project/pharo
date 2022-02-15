"
I am a command to open the breakpoint menu after a right click on the source code.
I show in the menu all commands annotated by SycReflectiveActivation
"
Class {
	#name : #SycOpenReflectivityMenuCommand,
	#superclass : #SycOpenSourceCodeMenuCommand,
	#category : #'SystemCommands-SourceCodeCommands'
}

{ #category : #execution }
SycOpenReflectivityMenuCommand >> activationStrategy [
	^SycReflectivityMenuActivation
]

{ #category : #accessing }
SycOpenReflectivityMenuCommand >> defaultMenuIconName [
	^ #smallObjects
]

{ #category : #accessing }
SycOpenReflectivityMenuCommand >> defaultMenuItemName [

	^ 'Reflectivity'
]
