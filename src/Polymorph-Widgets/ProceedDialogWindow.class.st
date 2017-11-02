"
Yes/no dialog. Test result as to whether the dialog is cancelled (no) or not (yes).
"
Class {
	#name : #ProceedDialogWindow,
	#superclass : #MessageDialogWindow,
	#category : 'Polymorph-Widgets-Windows'
}

{ #category : #icons }
ProceedDialogWindow class >> taskbarIconName [
	"Answer the icon for the receiver in a task bar."

	^#smallQuestionIcon
]

{ #category : #'as yet unclassified' }
ProceedDialogWindow >> handlesKeyboard: evt [
	"Return true if the receiver wishes to handle the given keyboard event"
	
	(super handlesKeyboard: evt) ifTrue: [^true].
	^evt keyCharacter = $y or: [
		evt keyCharacter = $n]
	
]

{ #category : #'as yet unclassified' }
ProceedDialogWindow >> icon [
	"Answer an icon for the receiver."

	^ self iconNamed: #questionIcon
]

{ #category : #'as yet unclassified' }
ProceedDialogWindow >> keyStroke: evt [
	"Additionally check for y and n keys (aliases for ok and cancel)."

	(super keyStroke: evt) ifTrue: [^true].
	evt keyCharacter = $y ifTrue: [self yes. ^true].
	evt keyCharacter = $n ifTrue: [self no. ^true].
	^false
]

{ #category : #'as yet unclassified' }
ProceedDialogWindow >> newButtons [
	"Answer new buttons as appropriate."

	^{self newOKButton. self newCancelButton isDefault: true}
]

{ #category : #'as yet unclassified' }
ProceedDialogWindow >> no [
	"Answer no."

	self cancel
]

{ #category : #'as yet unclassified' }
ProceedDialogWindow >> yes [
	"Answer yes."

	self ok
]
