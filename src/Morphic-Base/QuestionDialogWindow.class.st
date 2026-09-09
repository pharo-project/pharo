"
A yes/no/cancel message dialog. Cancel (escape & return) is the default.
"
Class {
	#name : 'QuestionDialogWindow',
	#superclass : 'ProceedDialogWindow',
	#instVars : [
		'answer'
	],
	#category : 'Morphic-Base-Windows',
	#package : 'Morphic-Base',
	#tag : 'Windows'
}

{ #category : 'accessing' }
QuestionDialogWindow >> answer [
	"Answer the value of answer"

	^ answer
]

{ #category : 'accessing' }
QuestionDialogWindow >> answer: anObject [
	"Set the value of answer"

	answer := anObject
]

{ #category : 'accessing' }
QuestionDialogWindow >> newButtons [
	"Answer new buttons as appropriate."

	^ {
		self newCancelButton.
		self newNoButton. 
		self newYesButton isDefault: true 
	}
]

{ #category : 'accessing' }
QuestionDialogWindow >> no [
	"Answer no."

	self
		answer: false;
		ok
]

{ #category : 'accessing' }
QuestionDialogWindow >> yes [
	"Answer yes."

	self
		answer: true;
		ok
]
