"
Message dialog with a warning icon.
"
Class {
	#name : #AlertDialogWindow,
	#superclass : #MessageDialogWindow,
	#category : #Polymorph-Widgets-Windows
}

{ #category : #icons }
AlertDialogWindow class >> taskbarIconName [
	"Answer the icon for the receiver in a task bar."

	^ #smallWarningIcon
]

{ #category : #'visual properties' }
AlertDialogWindow >> icon [
	"Answer an icon for the receiver."

	^ self iconNamed: #warningIcon
]
