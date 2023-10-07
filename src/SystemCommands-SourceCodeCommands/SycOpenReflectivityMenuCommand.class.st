"
I am a command to open the breakpoint menu after a right click on the source code.
I show in the menu all commands annotated by SycReflectiveActivation
"
Class {
	#name : 'SycOpenReflectivityMenuCommand',
	#superclass : 'SycOpenContextMenuCommand',
	#category : 'SystemCommands-SourceCodeCommands',
	#package : 'SystemCommands-SourceCodeCommands'
}

{ #category : 'execution' }
SycOpenReflectivityMenuCommand >> activationStrategy [
	^SycReflectivityMenuActivation
]

{ #category : 'context menu' }
SycOpenReflectivityMenuCommand >> cmCommandClass [

	^ SycSourceCodeCmCommand
]

{ #category : 'accessing' }
SycOpenReflectivityMenuCommand >> defaultMenuIconName [
	^ #smallObjects
]

{ #category : 'accessing' }
SycOpenReflectivityMenuCommand >> defaultMenuItemName [

	^ 'Reflectivity'
]
