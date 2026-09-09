"
A yes/no message dialog. Yes (return) is the default. Escape will answer no.
"
Class {
	#name : 'QuestionWithoutCancelDialogWindow',
	#superclass : 'QuestionDialogWindow',
	#category : 'Morphic-Base-Windows',
	#package : 'Morphic-Base',
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

	^ {
		self newNoButton.
		self newYesButton isDefault: true
	}
]
