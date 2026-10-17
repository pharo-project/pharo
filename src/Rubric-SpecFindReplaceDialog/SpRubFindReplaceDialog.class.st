"
I am the find and replace service used by the Rubric text areas that support find and replace.

I open the Spec dialog SpRubFindReplaceDialog and forward the find and replace requests to it.

The IDE baseline installs me as the default service of RubEditingArea and RubTextFieldArea (see BaselineOfIDE); text areas without find and replace support use RubNullFindReplaceService instead.
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
		'replaceTextInput'
	],
	#classInstVars : [
		'textToFind',
		'replacingText'
	],
	#category : 'Rubric-SpecFindReplaceDialog',
	#package : 'Rubric-SpecFindReplaceDialog'
}

{ #category : 'accessing' }
SpRubFindReplaceDialog class >> replacingText [

    ^ replacingText ifNil: [ replacingText := '' ]
]

{ #category : 'accessing' }
SpRubFindReplaceDialog class >> replacingText: aText [

	replacingText := aText
]

{ #category : 'accessing' }
SpRubFindReplaceDialog class >> textToFind [

    ^ textToFind ifNil: [ textToFind := '' ]
]

{ #category : 'accessing' }
SpRubFindReplaceDialog class >> textToFind: aText [

	textToFind := aText
]

{ #category : 'initialization' }
SpRubFindReplaceDialog >> connectPresenters [

	findInput
		whenTextChangedDo: [ :aText | self updateFindText: aText ];
		whenSubmitDo: [ :text | self find ].

	replaceTextInput whenTextChangedDo: [ :aText | self replacingText: aText ].

	caseCheckBox whenChangedDo: [ :aBoolean | service caseSensitive: aBoolean ].
	regExpCheckBox whenChangedDo: [ :aBoolean | service isRegex: aBoolean ].
	entireCheckBox whenChangedDo: [ :aBoolean | service entireWordsOnly: aBoolean ].
	wrapCheckBox whenChangedDo: [ :aBoolean | service wrapAround: aBoolean ].
	backwardsCheckBox whenChangedDo: [ :aBoolean | service searchBackwards: aBoolean ]
]

{ #category : 'layout' }
SpRubFindReplaceDialog >> defaultLayout [

	<spec>
	^ SpBoxLayout newVertical
		  add: findInput expand: false;
		  add: (SpGridLayout new
				   beColumnHomogeneous;
				   beRowHomogeneous;
				   add: regExpCheckBox atPoint: 1 @ 1;
				   add: backwardsCheckBox atPoint: 2 @ 1;
				   add: caseCheckBox atPoint: 1 @ 2;
				   add: wrapCheckBox atPoint: 2 @ 2;
				   add: entireCheckBox atPoint: 1 @ 3;
				   yourself);
		  addLast: (SpBoxLayout newHorizontal
				   spacing: 5;
				   add: 'Replace with:' expand: false;
				   add: replaceTextInput;
				   yourself);
		  yourself
]

{ #category : 'operations' }
SpRubFindReplaceDialog >> find [
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

	findButton := aDialogWindowPresenter addButton: 'Find' do: [ :presenter |
			              self find.
			              self textToFind: findInput text ].
	replaceButton := aDialogWindowPresenter addButton: 'Replace' do: [ :presenter |
			                 self replace.
			                 self replacingText: replaceTextInput text ].
	replaceAllButton := aDialogWindowPresenter addButton: 'Replace All' do: [ :presenter |
			                    self replaceAll.
			                    self replacingText: replaceTextInput text ].
	closeButton := aDialogWindowPresenter addButton: 'Close' do: [ :presenter |
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
	aDialogWindowPresenter whenOpenedDo: [ findInput selectAll ].
	aDialogWindowPresenter whenClosedDo: [
			service findText: ''.
			service discardDialog ]
]

{ #category : 'initialization' }
SpRubFindReplaceDialog >> initializePresenters [

	findInput := self newTextInput
		             autoAccept: true;
		             placeholder: 'Text to find'.

	findInput text: self textToFind.
	self updateFindText: self textToFind.

	regExpCheckBox := self newCheckBox label: 'Regular expression'.
	backwardsCheckBox := self newCheckBox label: 'Search backwards'.
	caseCheckBox := self newCheckBox label: 'Case sensitive'.
	wrapCheckBox := self newCheckBox label: 'Wrap around'.
	entireCheckBox := self newCheckBox label: 'Entire words only'.
	self updateFromService.
	replaceTextInput := self newTextInput autoAccept: true.
	self selectAllOnFocusIn: replaceTextInput.

	replaceTextInput text: self replacingText.
	service replaceText: replaceTextInput text.

	self focusOrder
		add: findInput;
		add: replaceTextInput;
		add: regExpCheckBox;
		add: backwardsCheckBox;
		add: caseCheckBox;
		add: wrapCheckBox;
		add: entireCheckBox
]

{ #category : 'private' }
SpRubFindReplaceDialog >> newReplacement [
	service replaceText: replaceTextInput text asString
]

{ #category : 'operations' }
SpRubFindReplaceDialog >> replace [
	self newReplacement.
	service replace.
	service find
]

{ #category : 'operations' }
SpRubFindReplaceDialog >> replaceAll [
	self newReplacement.
	service replaceAll
]

{ #category : 'accessing' }
SpRubFindReplaceDialog >> replacingText [

	^ self class replacingText
]

{ #category : 'operations' }
SpRubFindReplaceDialog >> replacingText: aText [

	^ self class replacingText: aText
]

{ #category : 'initialization' }
SpRubFindReplaceDialog >> selectAllOnFocusIn: aPresenter [

	aPresenter eventHandler whenFocusReceivedDo: [ :anEvent | replaceTextInput selectAll ]
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

{ #category : 'operations' }
SpRubFindReplaceDialog >> updateFindText: aText [

    service findText: aText.
    self class textToFind: aText
]

{ #category : 'initialization' }
SpRubFindReplaceDialog >> updateFromModel [

	findInput text: (self service findText
		ifEmpty: [ '' ]
		ifNotEmpty: [ :aText | aText asString lines first ])
]

{ #category : 'initialization' }
SpRubFindReplaceDialog >> updateFromService [

	caseCheckBox state: service caseSensitive.
	regExpCheckBox state: service isRegex.
	entireCheckBox state: service entireWordsOnly.
	wrapCheckBox state: service wrapAround.
	backwardsCheckBox state: service searchBackwards
]
