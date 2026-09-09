"
Message dialog with a warning icon.
"
Class {
	#name : 'AlertDialogWindow',
	#superclass : 'MessageDialogWindow',
	#category : 'Morphic-Base-Windows',
	#package : 'Morphic-Base',
	#tag : 'Windows'
}

{ #category : 'icons' }
AlertDialogWindow class >> taskbarIconName [
	"Answer the icon for the receiver in a task bar."

	^ #smallWarning
]

{ #category : 'visual properties' }
AlertDialogWindow >> icon [
	"Answer an icon for the receiver."

	^ self iconNamed: #warning
]
