Class {
	#name : #SpecRubDialog,
	#superclass : #SpPresenter,
	#instVars : [
		'service',
		'findInput',
		'findButton',
		'regExpCheckBox',
		'backwardsCheckBox',
		'caseCheckBox',
		'wrapCheckBox',
		'entireCheckBox',
		'replaceButton',
		'replaceAllButton',
		'cancelButton',
		'updateFromModel'
	],
	#category : #'Rubric-Editing-FindReplaceService'
}

{ #category : #specs }
SpecRubDialog class >> defaultSpec [
	<spec>
	^ SpBoxLayout newVertical
		add: #findInput;
		add: (SpGridLayout new 
			beColumnHomogeneous;
			beRowHomogeneous;
			add: #regExpCheckBox at: 1@1;
			add: #backwardsCheckBox at: 2@1;
			add: #caseCheckBox at: 1@2;
			add: #wrapCheckBox at: 2@2;
			add: #entireCheckBox at: 1@3;
			yourself);
		addLast: (SpBoxLayout newHorizontal 
			borderWidth: 5;
			spacing: 5;
			add: #findButton;
			add: #replaceButton;
			add: #replaceAllButton;
			add: #cancelButton);
		yourself
]

{ #category : #accessing }
SpecRubDialog >> findButton [
	^ findButton
]

{ #category : #accessing }
SpecRubDialog >> findButton: anObject [
	findButton := anObject
]

{ #category : #accessing }
SpecRubDialog >> findInput [
	^ findInput
]

{ #category : #accessing }
SpecRubDialog >> findInput: anObject [
	findInput := anObject.
]

{ #category : #initialization }
SpecRubDialog >> initializePresenters [
	findInput := self newTextInput autoAccept: true; placeholder: 'Text to find'.
	findInput whenTextChangedDo: [ self updateFindText ].
	
	regExpCheckBox := self newCheckBox label: 'Regular expression'.
	backwardsCheckBox := self newCheckBox label: 'Search backwards'.
	caseCheckBox := self newCheckBox label: 'Case sensitive'.
	wrapCheckBox := self newCheckBox label: 'Wrap around'.
	entireCheckBox := self newCheckBox label: 'Entire words only'.
	
	findButton := self newButton label: 'Find'.
	findButton action: [ service find ].
	
	replaceButton := self newButton label: 'Replace';
		action: [ service find ].
	replaceAllButton := self newButton label: 'Replace';
		action: [ service find ].
	cancelButton := self newButton label: 'Replace all';
		action: [ service find ].
	updateFromModel := self newButton label: 'Cancel';
		action: [ service find ].
]

{ #category : #accessing }
SpecRubDialog >> service [
	^ service
]

{ #category : #accessing }
SpecRubDialog >> service: anObject [
	service := anObject
]

{ #category : #initialization }
SpecRubDialog >> setModelBeforeInitialization: aService [

	service := aService
]

{ #category : #initialization }
SpecRubDialog >> updateFindText [

	#updateFindText traceCr.
	service findText: self findInput text asString.
]

{ #category : #initialization }
SpecRubDialog >> updateFromModel [

	findInput text: self service findText.


]
