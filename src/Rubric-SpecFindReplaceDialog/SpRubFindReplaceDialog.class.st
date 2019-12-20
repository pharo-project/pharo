"
I am a Spec dialog used for find & replace service. I am currently partly dependent on Morphic backend because of more user friendly keyboard manipulation.
"
Class {
	#name : #SpRubFindReplaceDialog,
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
	#category : #'Rubric-SpecFindReplaceDialog'
}

{ #category : #specs }
SpRubFindReplaceDialog class >> defaultSpec [
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

{ #category : #'morphic dependencies' }
SpRubFindReplaceDialog >> assignEnterKeystrokeEventFor: aPresenter [

	"Morphic dependent!"

	aPresenter whenBuiltDo: [ :this | this widget announcer when: RubReturnEntered do: [ :ann |
		self find ] ].
]

{ #category : #'morphic dependencies' }
SpRubFindReplaceDialog >> closeWindowOnEscIn: aPresenter [

	"Morphic dependent!"

	aPresenter whenBuiltDo: [ :this | this widget announcer when: RubKeystroke do: [ :ann |
		ann event keyCharacter = Character escape ifTrue: [ self window triggerCancelAction ] ] ].
]

{ #category : #operations }
SpRubFindReplaceDialog >> find [ 
	self updateService.
	service find
]

{ #category : #accessing }
SpRubFindReplaceDialog >> findButton [
	^ findButton
]

{ #category : #accessing }
SpRubFindReplaceDialog >> findButton: anObject [
	findButton := anObject
]

{ #category : #accessing }
SpRubFindReplaceDialog >> findInput [
	^ findInput
]

{ #category : #accessing }
SpRubFindReplaceDialog >> findInput: anObject [
	findInput := anObject.
]

{ #category : #initialization }
SpRubFindReplaceDialog >> initializeDialogWindow: aDialogWindowPresenter [
	
	findButton := aDialogWindowPresenter addButton: 'Find' do: [ :presenter | 
			self find ].
	replaceButton := aDialogWindowPresenter addButton: 'Replace' do: [ :presenter | 
			self replace ].
	replaceAllButton := aDialogWindowPresenter addButton: 'Replace All' do: [ :presenter | 
			self replaceAll ].
	cancelButton := aDialogWindowPresenter addButton: 'Cancel' do: [ :presenter | 
			presenter triggerCancelAction.
			presenter close ].
		
	self focusOrder 
		add: findButton;
		add: replaceButton;
		add: replaceAllButton;
		add: cancelButton.
				
	aDialogWindowPresenter
		title: self title;
		initialExtent: 430 @ 220.
		
	aDialogWindowPresenter whenClosedDo: [ service discardDialog ]

]

{ #category : #initialization }
SpRubFindReplaceDialog >> initializePresenters [
	findInput := self newTextInput autoAccept: true; placeholder: 'Text to find'.
	
	findInput whenTextChangedDo: [ self updateFindText ].
	
	self closeWindowOnEscIn: findInput.
	self assignEnterKeystrokeEventFor: findInput.
	
	regExpCheckBox := self newCheckBox label: 'Regular expression'.
	backwardsCheckBox := self newCheckBox label: 'Search backwards'.
	caseCheckBox := self newCheckBox label: 'Case sensitive'.
	wrapCheckBox := self newCheckBox label: 'Wrap around'.
	entireCheckBox := self newCheckBox label: 'Entire words only'.
			
	replaceTextInput := self newTextInput autoAccept: true.
	self closeWindowOnEscIn: replaceTextInput.

	replaceLabel := self newLabel label: 'Replace with:'.
	
	self focusOrder 
		add: findInput;
		add: replaceTextInput;
		add: regExpCheckBox;
		add: backwardsCheckBox;
		add: caseCheckBox;
		add: wrapCheckBox;
		add: entireCheckBox.
		
]

{ #category : #private }
SpRubFindReplaceDialog >> newReplacement [
	| t |
	(t := replaceTextInput text asString) ifEmpty: [^ self ].
	
	service replaceText: replaceTextInput text asString.

]

{ #category : #operations }
SpRubFindReplaceDialog >> replace [
	self updateService.
	self newReplacement.
	service replace.
	service find
]

{ #category : #operations }
SpRubFindReplaceDialog >> replaceAll [
	self updateService.
	self newReplacement.
	service replaceAll.
]

{ #category : #accessing }
SpRubFindReplaceDialog >> service [
	^ service
]

{ #category : #accessing }
SpRubFindReplaceDialog >> service: anObject [
	service := anObject
]

{ #category : #initialization }
SpRubFindReplaceDialog >> setModelBeforeInitialization: aService [

	service := aService
]

{ #category : #accessing }
SpRubFindReplaceDialog >> title [ 
	
	^ 'Find & Replace'
]

{ #category : #initialization }
SpRubFindReplaceDialog >> updateFindText [

	#updateFindText traceCr.
	service findText: self findInput text asString.
]

{ #category : #initialization }
SpRubFindReplaceDialog >> updateFromModel [

	findInput text: (self service findText
		ifEmpty: [ '' ]
		ifNotEmpty: [ :aText | aText asString lines first ]).


]

{ #category : #private }
SpRubFindReplaceDialog >> updateService [

	service caseSensitive: caseCheckBox state.
	service isRegex: regExpCheckBox state.
	service entireWordsOnly: entireCheckBox state.
	service wrapAround: wrapCheckBox state.
	service searchBackwards: backwardsCheckBox state.
	
]
