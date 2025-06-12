"
I am a Spec dialog used for find & replace service. I am currently partly dependent on Morphic backend because of more user friendly keyboard manipulation.
"
Class {
	#name : 'SpRubFindReplaceDialog',
	#superclass : 'SpPresenter',
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
		'closeButton',
		'replaceTextInput',
		'replaceLabel'
	],
	#classInstVars : [
		'textToFind',
		'replacingText'
	],
	#category : 'Rubric-SpecFindReplaceDialog',
	#package : 'Rubric-SpecFindReplaceDialog'
}

{ #category : 'layout' }
SpRubFindReplaceDialog class >> defaultLayout [

	<spec>
	^ SpBoxLayout newVertical
		  add: #findInput expand: false;
		  add: (SpGridLayout new
				   beColumnHomogeneous;
				   beRowHomogeneous;
				   add: #regExpCheckBox atPoint: 1 @ 1;
				   add: #backwardsCheckBox atPoint: 2 @ 1;
				   add: #caseCheckBox atPoint: 1 @ 2;
				   add: #wrapCheckBox atPoint: 2 @ 2;
				   add: #entireCheckBox atPoint: 1 @ 3;
				   yourself);
		  addLast: (SpBoxLayout newHorizontal
				   borderWidth: 5;
				   spacing: 5;
				   add: #replaceLabel
				   expand: false
				   fill: false
				   padding: 0;
				   add: #replaceTextInput;
				   yourself);
		  "addLast: (SpBoxLayout newHorizontal
			borderWidth: 3;
			spacing: 5;
			add: #findButton;
			add: #replaceButton;
			add: #replaceAllButton;
			add: #cancelButton);"
			  yourself
]

{ #category : 'accessing' }
SpRubFindReplaceDialog class >> replacingText [

	^ replacingText ifNil: [ replacingText := nil ]
]

{ #category : 'accessing' }
SpRubFindReplaceDialog class >> replacingText: aText [

	replacingText := aText
]

{ #category : 'accessing' }
SpRubFindReplaceDialog class >> textToFind [

	^ textToFind ifNil: [ textToFind := nil ]
]

{ #category : 'accessing' }
SpRubFindReplaceDialog class >> textToFind: aText [

	textToFind := aText
]

{ #category : 'morphic dependencies' }
SpRubFindReplaceDialog >> assignEnterKeystrokeEventFor: aPresenter [
	"Morphic dependent!"

	aPresenter whenBuiltDo: [ :this | this widget announcer when: RubReturnEntered do: [ :ann | self find ] for: self ]
]

{ #category : 'morphic dependencies' }
SpRubFindReplaceDialog >> closeWindowOnEscIn: aPresenter [
	"Morphic dependent!"

	aPresenter whenBuiltDo: [ :this |
		this widget announcer when: RubKeystroke do: [ :ann | ann event keyCharacter = Character escape ifTrue: [ self window triggerCancelAction ] ] for: self ]
]

{ #category : 'initialization' }
SpRubFindReplaceDialog >> connectPresenters [

	findInput whenTextChangedDo: [ self updateFindText ].
	replaceTextInput whenTextChangedDo: [ :aText | self replacingText: aText ]
]

{ #category : 'operations' }
SpRubFindReplaceDialog >> find [
	self updateService.
	service find
]

{ #category : 'accessing' }
SpRubFindReplaceDialog >> findButton [
	^ findButton
]

{ #category : 'accessing' }
SpRubFindReplaceDialog >> findButton: anObject [
	findButton := anObject
]

{ #category : 'accessing' }
SpRubFindReplaceDialog >> findInput [
	^ findInput
]

{ #category : 'accessing' }
SpRubFindReplaceDialog >> findInput: anObject [
	findInput := anObject
]

{ #category : 'initialization' }
SpRubFindReplaceDialog >> initializeDialogWindow: aDialogWindowPresenter [

	findButton := aDialogWindowPresenter
		              addButton: 'Find'
		              do: [ :presenter |
			              self find.
			              self textToFind: findInput text ].
	replaceButton := aDialogWindowPresenter
		                 addButton: 'Replace'
		                 do: [ :presenter |
			                 self replace.
			                 self replacingText: replaceTextInput text ].
	replaceAllButton := aDialogWindowPresenter
		                    addButton: 'Replace All'
		                    do: [ :presenter |
			                    self replaceAll.
			                    self replacingText: replaceTextInput text ].
	closeButton := aDialogWindowPresenter
		               addButton: 'Close'
		               do: [ :presenter |
			               presenter triggerCancelAction.
			               presenter close ].

	self focusOrder
		add: findButton;
		add: replaceButton;
		add: replaceAllButton;
		add: closeButton.

	aDialogWindowPresenter
		title: self title;
		initialExtent: 430 @ 220.

	aDialogWindowPresenter whenClosedDo: [
		service findText: ''.
		service discardDialog ]
]

{ #category : 'initialization' }
SpRubFindReplaceDialog >> initializePresenters [

	findInput := self newTextInput
		             autoAccept: true;
		             placeholder: 'Text to find'.
	self textToFind ifNotNil: [
		findInput text: self textToFind.
		self updateFindText ].

	self closeWindowOnEscIn: findInput.
	self assignEnterKeystrokeEventFor: findInput.

	regExpCheckBox := self newCheckBox label: 'Regular expression'.
	backwardsCheckBox := self newCheckBox label: 'Search backwards'.
	caseCheckBox := self newCheckBox label: 'Case sensitive'.
	wrapCheckBox := self newCheckBox label: 'Wrap around'.
	entireCheckBox := self newCheckBox label: 'Entire words only'.

	replaceTextInput := self newTextInput autoAccept: true.
	self closeWindowOnEscIn: replaceTextInput.
	self replacingText ifNotNil: [
		replaceTextInput text: self replacingText.
		service replaceText: replaceTextInput text ].

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

{ #category : 'private' }
SpRubFindReplaceDialog >> newReplacement [
	service replaceText: replaceTextInput text asString
]

{ #category : 'operations' }
SpRubFindReplaceDialog >> replace [
	self updateService.
	self newReplacement.
	service replace.
	service find
]

{ #category : 'operations' }
SpRubFindReplaceDialog >> replaceAll [
	self updateService.
	self newReplacement.
	service replaceAll
]

{ #category : 'accessing' }
SpRubFindReplaceDialog >> replacingText [

	^ self class replacingText
]

{ #category : 'accessing' }
SpRubFindReplaceDialog >> replacingText: aText [

	^ self class replacingText: aText
]

{ #category : 'accessing' }
SpRubFindReplaceDialog >> service [
	^ service
]

{ #category : 'accessing' }
SpRubFindReplaceDialog >> service: anObject [
	service := anObject
]

{ #category : 'initialization' }
SpRubFindReplaceDialog >> setModelBeforeInitialization: aService [

	service := aService
]

{ #category : 'accessing' }
SpRubFindReplaceDialog >> textToFind [

	^ self class textToFind
]

{ #category : 'accessing' }
SpRubFindReplaceDialog >> textToFind: aText [

	^ self class textToFind: aText
]

{ #category : 'accessing' }
SpRubFindReplaceDialog >> title [

	^ 'Find & Replace'
]

{ #category : 'initialization' }
SpRubFindReplaceDialog >> updateFindText [

	service findText: self findInput text asString
]

{ #category : 'initialization' }
SpRubFindReplaceDialog >> updateFromModel [

	findInput text: (self service findText
		ifEmpty: [ '' ]
		ifNotEmpty: [ :aText | aText asString lines first ])
]

{ #category : 'private' }
SpRubFindReplaceDialog >> updateService [

	service caseSensitive: caseCheckBox state.
	service isRegex: regExpCheckBox state.
	service entireWordsOnly: entireCheckBox state.
	service wrapAround: wrapCheckBox state.
	service searchBackwards: backwardsCheckBox state
]
