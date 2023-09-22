"
Message dialog with a warning icon.
"
Class {
	#name : 'AlertDialogWindow',
	#superclass : 'MessageDialogWindow',
	#category : 'Polymorph-Widgets-Windows',
	#package : 'Polymorph-Widgets',
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

	^ self iconNamed: #warningIcon
]
