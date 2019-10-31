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
		'updateFromModel',
		'replaceTextInput',
		'replaceLabel'
	],
	#category : #'Rubric-Editing-FindReplaceService'
}

{ #category : #specs }
SpecRubDialog class >> defaultSpec [
	<spec>
	^ SpBoxLayout newVertical
		add: #findInput
			expand: false fill: false padding: 0;
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
			add: #replaceLabel  expand: false fill: false padding: 0;
			add: #replaceTextInput);
		"addLast: (SpBoxLayout newHorizontal 
			borderWidth: 3;
			spacing: 5;
			add: #findButton;
			add: #replaceButton;
			add: #replaceAllButton;
			add: #cancelButton);"
		yourself
]

{ #category : #accessing }
SpecRubDialog >> closeWindowOnEscIn: aPresenter [

	"Morphic dependent!"

	aPresenter whenBuiltDo: [ :this | this widget announcer when: RubKeystroke do: [ :ann |
		ann event keyCharacter = Character escape ifTrue: [ self window triggerCancelAction ] ] ].
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

{ #category : #accessing }
SpecRubDialog >> initializeDialogWindow: aDialogWindowPresenter [
	
	findButton := aDialogWindowPresenter addButton: 'Find' do: [ :presenter | 
			service find ].
	replaceButton := aDialogWindowPresenter addButton: 'Replace' do: [ :presenter | 
			service find ].
	replaceAllButton := aDialogWindowPresenter addButton: 'Replace All' do: [ :presenter | 
			service find ].
	cancelButton := aDialogWindowPresenter addButton: 'Cancel' do: [ :presenter | 
			presenter triggerCancelAction.
			presenter close ].
				
	aDialogWindowPresenter
		title: self title;
		initialExtent: 430 @ 220
]

{ #category : #initialization }
SpecRubDialog >> initializePresenters [
	findInput := self newTextInput autoAccept: true; placeholder: 'Text to find'.
	
	findInput whenTextChangedDo: [ self updateFindText ].
	
	self closeWindowOnEscIn: findInput.
	
	regExpCheckBox := self newCheckBox label: 'Regular expression'.
	backwardsCheckBox := self newCheckBox label: 'Search backwards'.
	caseCheckBox := self newCheckBox label: 'Case sensitive'.
	wrapCheckBox := self newCheckBox label: 'Wrap around'.
	entireCheckBox := self newCheckBox label: 'Entire words only'.
	
	findButton := self newButton label: 'Find'.
	findButton action: [ service find ].
	
	replaceButton := self newButton label: 'Replace';
		action: [ service find ].
	replaceAllButton := self newButton label: 'Replace All';
		action: [ service find ].
	cancelButton := self newButton label: 'Cancel';
		action: [ service find ].
		
	replaceTextInput := self newTextInput autoAccept: true.
	self closeWindowOnEscIn: replaceTextInput.

	replaceLabel := self newLabel label: 'Replace with:'.
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

{ #category : #accessing }
SpecRubDialog >> title [ 
	
	^ 'Find & Replace'
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
