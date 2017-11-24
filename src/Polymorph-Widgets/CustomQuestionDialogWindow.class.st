"
QuestionDialog supporting custom text/buttons for yes/no choices.
"
Class {
	#name : #CustomQuestionDialogWindow,
	#superclass : #QuestionDialogWindow,
	#instVars : [
		'yesButton',
		'noButton',
		'cancelButton'
	],
	#category : #'Polymorph-Widgets-Windows'
}

{ #category : #accessing }
CustomQuestionDialogWindow >> cancelButton [

	^ cancelButton
]

{ #category : #accessing }
CustomQuestionDialogWindow >> cancelButton: anObject [

	cancelButton := anObject
]

{ #category : #'accessing - text' }
CustomQuestionDialogWindow >> cancelText: aStringOrText help: helpString [
	"Set the cancel button label.
	if the text is nil, remove the button."

	aStringOrText
		ifNil: [self cancelButton delete]
		ifNotNil: [self cancelButton
					hResizing: #shrinkWrap;
					label: aStringOrText;
					setBalloonText: helpString]
]

{ #category : #defaults }
CustomQuestionDialogWindow >> default: defaultOption [
	"Set the default button."

	defaultOption
		ifNil: [self cancelButton isDefault: true]
		ifNotNil: [defaultOption
					ifTrue: [self yesButton isDefault: true]
					ifFalse: [self noButton isDefault: true]]
]

{ #category : #defaults }
CustomQuestionDialogWindow >> defaultCancelButton [
	"Answer a default cancel button."

	^self newCancelButton
]

{ #category : #defaults }
CustomQuestionDialogWindow >> defaultNoButton [
	"Answer a default no button."

	^self newNoButton
]

{ #category : #defaults }
CustomQuestionDialogWindow >> defaultYesButton [
	"Answer a default yes button."

	^self newYesButton
]

{ #category : #initialization }
CustomQuestionDialogWindow >> initialize [
	"Initialize the receiver."

	self
		yesButton: self defaultYesButton;
		noButton: self defaultNoButton;
		cancelButton: self defaultCancelButton.
	super initialize
]

{ #category : #building }
CustomQuestionDialogWindow >> newButtons [
	"Answer new buttons as appropriate."

	^{self yesButton. self noButton. self cancelButton}
]

{ #category : #accessing }
CustomQuestionDialogWindow >> noButton [
	"Answer the value of noButton"

	^ noButton
]

{ #category : #accessing }
CustomQuestionDialogWindow >> noButton: anObject [
	"Set the value of noButton"

	noButton := anObject
]

{ #category : #'accessing - text' }
CustomQuestionDialogWindow >> noText: aStringOrText help: helpString [
	"Set the no button label.
	if the text is nil, remove the button."

	aStringOrText
		ifNil: [self noButton delete]
		ifNotNil: [self noButton
					hResizing: #shrinkWrap;
					label: aStringOrText;
					setBalloonText: helpString]
]

{ #category : #accessing }
CustomQuestionDialogWindow >> yesButton [
	"Answer the value of yesButton"

	^ yesButton
]

{ #category : #accessing }
CustomQuestionDialogWindow >> yesButton: anObject [
	"Set the value of yesButton"

	yesButton := anObject
]

{ #category : #'accessing - text' }
CustomQuestionDialogWindow >> yesText: aStringOrText help: helpString [
	"Set the yes button label.
	if the text is nil, remove the button."

	aStringOrText
		ifNil: [self cancelButton delete]
		ifNotNil: [self yesButton
					hResizing: #shrinkWrap;
					label: aStringOrText;
					setBalloonText: helpString]
]
