"
A message dialog with an error icon.
"
Class {
	#name : 'ErrorDialogWindow',
	#superclass : 'MessageDialogWindow',
	#category : 'Morphic-Base-Windows',
	#package : 'Morphic-Base',
	#tag : 'Windows'
}

{ #category : 'icons' }
ErrorDialogWindow class >> taskbarIconName [
	"Answer the icon for the receiver in a task bar."

	^#smallError
]

{ #category : 'accessing' }
ErrorDialogWindow >> icon [
	"Answer an icon for the receiver."

	^ self iconNamed: #error
]
