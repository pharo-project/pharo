"
A yes/no message dialog. Yes (return) is the default. Escape will answer no.
"
Class {
	#name : 'QuestionWithoutCancelDialogWindow',
	#superclass : 'QuestionDialogWindow',
	#classTraits : '{} + TraitedClass',
	#category : 'Polymorph-Widgets-Windows',
	#package : 'Polymorph-Widgets',
	#tag : 'Windows'
}

{ #category : 'keymapping' }
QuestionWithoutCancelDialogWindow >> closeWindowAction [

	self escapePressed
]

{ #category : 'actions' }
QuestionWithoutCancelDialogWindow >> escapePressed [
	"Default is to cancel."

	self no
]

{ #category : 'accessing' }
QuestionWithoutCancelDialogWindow >> newButtons [
	"Answer new buttons as appropriate."

	^{self newYesButton isDefault: true. self newNoButton}
]
