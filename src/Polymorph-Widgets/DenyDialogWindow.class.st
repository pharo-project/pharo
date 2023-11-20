"
Dialog window displaying a message with a single OK button. Escape/return will close. Icon is a themed lock icon.
"
Class {
	#name : 'DenyDialogWindow',
	#superclass : 'MessageDialogWindow',
	#category : 'Polymorph-Widgets-Windows',
	#package : 'Polymorph-Widgets',
	#tag : 'Windows'
}

{ #category : 'icons' }
DenyDialogWindow class >> taskbarIconName [
	"Answer the icon for the receiver in a task bar."

	^#smallLock
]

{ #category : 'visual properties' }
DenyDialogWindow >> icon [
	"Answer an icon for the receiver."

	^ self iconNamed: #lockIcon
]
