"
i'm using to ask to user a String for something:
example:
	BISettingPreviewer use it to ask a name of a method that he want to creat

HOW to use in your UI:
1 - BIInputPresenterUI class method openOn:
2 - you have to define method 'acceptActionSBICodePresenter: aReferenceToYourUI ' 
	in this method you define the action of okButton 
	
Internal Representation and Key Implementation Points.

    Instance Variables
	codePresenter:		<InputTextPresenter>
	uiReceiver:		<SubClassOfComposablePresenter> 


    Implementation Points
"
Class {
	#name : #EFInputPresenterUI,
	#superclass : #SpPresenter,
	#instVars : [
		'uiReceiver',
		'codePresenter'
	],
	#category : #EnlumineurFormatterUI
}

{ #category : #specs }
EFInputPresenterUI class >> defaultSpec [
	^ SpBoxLayout newHorizontal 
		add: #codePresenter; yourself
]

{ #category : #'instance creation' }
EFInputPresenterUI class >> openOn: aUIReceiver [
	| biCodePresenter |
	biCodePresenter := self new.
	biCodePresenter uiReceiver: aUIReceiver.
	^ biCodePresenter openDialogWithSpec
]

{ #category : #accessing }
EFInputPresenterUI >> codePresenter [
	^ codePresenter
]

{ #category : #initialization }
EFInputPresenterUI >> initializeDialogWindow: aDialog [
	super initializeDialogWindow: aDialog.
	aDialog okAction: [ :presenter | uiReceiver acceptActionBICodePresenter: self  ]
]

{ #category : #initialization }
EFInputPresenterUI >> initializeWidgets [ 
	codePresenter := self instantiate: SpTextInputFieldPresenter.
	codePresenter placeholder: 'write the name of you configuration'.
	codePresenter autoAccept: true
]

{ #category : #accessing }
EFInputPresenterUI >> uiReceiver [
	^ uiReceiver
]

{ #category : #accessing }
EFInputPresenterUI >> uiReceiver: anUI [
	uiReceiver := anUI
]
