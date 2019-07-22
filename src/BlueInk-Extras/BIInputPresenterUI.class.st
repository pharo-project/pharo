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
	#name : #BIInputPresenterUI,
	#superclass : #ComposablePresenter,
	#instVars : [
		'uiReceiver',
		'codePresenter'
	],
	#category : #'BlueInk-Extras'
}

{ #category : #specs }
BIInputPresenterUI class >> defaultSpec [
	^ SpecBoxLayout newHorizontal 
		add: #codePresenter; yourself
]

{ #category : #'instance creation' }
BIInputPresenterUI class >> openOn: aUIReceiver [
	| biCodePresenter |
	biCodePresenter := self new.
	biCodePresenter uiReceiver: aUIReceiver.
	^ biCodePresenter openDialogWithSpec
]

{ #category : #accessing }
BIInputPresenterUI >> codePresenter [
	^ codePresenter
]

{ #category : #initialization }
BIInputPresenterUI >> initializeDialogWindow: aDialog [
	super initializeDialogWindow: aDialog.
	aDialog okAction: [ :presenter | uiReceiver acceptActionBICodePresenter: self  ]
]

{ #category : #initialization }
BIInputPresenterUI >> initializeWidgets [ 
	codePresenter := self instantiate: TextInputFieldPresenter.
	codePresenter placeholder: 'write the name of you configuration'.
	codePresenter autoAccept: true
]

{ #category : #accessing }
BIInputPresenterUI >> uiReceiver [
	^ uiReceiver
]

{ #category : #accessing }
BIInputPresenterUI >> uiReceiver: anUI [
	uiReceiver := anUI
]
