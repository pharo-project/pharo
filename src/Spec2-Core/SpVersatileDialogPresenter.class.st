"
A am versatile presenter for modal dialog windows with the customizable content

I have:
- title
- main message      - the basic text of the dialog 
- main icon         - the icon that is displayed next to the main message 
                      or next to the content area, if any 
- content area      - a text or another presenter with the detailed information of the dialog 
- buttons           - added using addButton:text:value
- more options text - a label of the button that shows more options of the dialog
- less options text - a label of the button that shows more options of the dialog in case
                      that the options are visible
- footnote icon     - an icon that is displayed next to the footnote 
- footnote area     - a text or another presenter with the footnote

"
Class {
	#name : #SpVersatileDialogPresenter,
	#superclass : #SpDynamicPresenter,
	#traits : 'SpTDynamicPresenter',
	#classTraits : 'SpTDynamicPresenter classTrait',
	#instVars : [
		'mainMessage',
		'mainIcon',
		'contentArea',
		'buttons',
		'moreOptionsButton',
		'moreOptionsText',
		'lessOptionsText',
		'moreOptionsArea',
		'footnoteIcon',
		'footnoteArea',
		'moreOptionsVisible',
		'mainMessagePresenter',
		'mainIconPresenter',
		'contentAreaPresenter',
		'moreOptionsAreaPresenter',
		'footnoteIconPresenter',
		'footnoteAreaPresenter',
		'result',
		'defaultButtonId'
	],
	#category : #'Spec2-Core-Widgets'
}

{ #category : #specs }
SpVersatileDialogPresenter class >> example1 [
	<example>
	| dialog |
	(dialog := SpVersatileDialogPresenter new)
		mainMessage: 'Confirm your choice' asText allBold;
		mainIcon: (self iconNamed: #question);
		contentArea: 'Do you want to discard your content?';
		moreOptionsArea: (dialog newCheckBox label: 'Discard permanently');
		addButton: #yes text: 'yes' value: #yes;
		addButton: #no text: 'no' value: #no;
		addButton: #cancel text: 'cancel' value: #cancel;
		footnoteIcon: (self iconNamed: #error);
		footnoteArea: 'This operation can be very dangerous.';
		openModalWithSpec.
	dialog withWindowDo: [ :w | w title: 'Confirmation' ].
	dialog result traceCr.

	self inform: dialog moreOptionsArea state asString
]

{ #category : #specs }
SpVersatileDialogPresenter class >> example2 [
	<example>
	| dialog |
	dialog := SpVersatileDialogPresenter new
		mainMessage: 'Save content' asText allBold;
		addButton: #save text: 'Save' value: true;
		addButton: #cancel text: 'Cancel' value: false;
		mainIcon: (self iconNamed: #question);
		openModalWithSpec;
		yourself.

	dialog withWindowDo: [ :w | w title: 'Confirmation' ].
	dialog result traceCr
]

{ #category : #specs }
SpVersatileDialogPresenter class >> example3 [
	<example>
	| dialog |
	dialog := SpVersatileDialogPresenter new
		mainMessage: 'Save content' asText allBold;
		addButton: #save text: 'Save' value: true;
		addButton: #cancel text: 'Cancel' value: false;
		mainIcon: (self iconNamed: #question);
		footnoteIcon: (self iconNamed: #error);
		footnoteArea: 'Saving may take several minutes';
		yourself.

	(dialog buttonAt: #save) icon: (self iconNamed: #smallSave).

	dialog openModalWithSpec.

	dialog withWindowDo: [ :w | w title: 'Confirmation' ].
	dialog result traceCr
]

{ #category : #specs }
SpVersatileDialogPresenter class >> example4 [
	<example>
	| dialog |
	dialog := SpVersatileDialogPresenter new.

	1 to: 5 do: [ :aNumber | dialog addButton: ('button' , aNumber asString) asSymbol text: aNumber printStringRoman value: aNumber ].

	dialog defaultButtonId: #button2.

	dialog openModalWithSpec.

	dialog withWindowDo: [ :w | w title: 'Select number' ].
	dialog result traceCr
]

{ #category : #specs }
SpVersatileDialogPresenter class >> example5 [
	<example>
	| dialog listPresenter |
	dialog := SpVersatileDialogPresenter new.
	listPresenter := dialog newList.
	listPresenter items: #(one two three).
	dialog contentArea: listPresenter.
	dialog
		addButton: #ok
		text: 'OK'
		value: true
		condition: [ listPresenter selection isEmpty not ].
	dialog addButton: #cancel text: 'Cancel' value: false.
	dialog mainIcon: (self iconNamed: #question).

	dialog openModalWithSpec.

	dialog withWindowDo: [ :w | w title: 'Confirmation' ].
	dialog result traceCr
]

{ #category : #api }
SpVersatileDialogPresenter >> addButton: id text: buttonText value: anObject [

	^ self addButton: id text: buttonText value: anObject condition: true
]

{ #category : #api }
SpVersatileDialogPresenter >> addButton: id text: buttonText value: anObject condition: aCondition [
	| aButton |
	aButton := self newButton label: buttonText.
	aButton action: [ aCondition value ifTrue: [ self return: anObject ] ].

	buttons at: id put: aButton.

	self presenterAt: id put: aButton
]

{ #category : #api }
SpVersatileDialogPresenter >> buttonAt: id [

	^ buttons at: id
]

{ #category : #accessing }
SpVersatileDialogPresenter >> buttons [

	^ buttons
]

{ #category : #initialization }
SpVersatileDialogPresenter >> connectPresenters [ 

	super connectPresenters.
	
	moreOptionsButton action: [ 
		moreOptionsVisible := moreOptionsVisible not.
		self needRebuild: false.
		self buildWithSpec ].
	

]

{ #category : #accessing }
SpVersatileDialogPresenter >> contentArea [

	^ contentArea
]

{ #category : #accessing }
SpVersatileDialogPresenter >> contentArea: aTextOrPresenter [

	contentArea := aTextOrPresenter.
]

{ #category : #accessing }
SpVersatileDialogPresenter >> contentAreaPresenter [
	^ contentAreaPresenter
]

{ #category : #accessing }
SpVersatileDialogPresenter >> defaultButtonId [
	^ defaultButtonId
]

{ #category : #accessing }
SpVersatileDialogPresenter >> defaultButtonId: anObject [
	defaultButtonId := anObject
]

{ #category : #accessing }
SpVersatileDialogPresenter >> footnoteArea [

	^ footnoteArea
]

{ #category : #accessing }
SpVersatileDialogPresenter >> footnoteArea: aTextOrPresenter [

	footnoteArea := aTextOrPresenter.
]

{ #category : #accessing }
SpVersatileDialogPresenter >> footnoteIcon [

	^ footnoteIcon
]

{ #category : #accessing }
SpVersatileDialogPresenter >> footnoteIcon: aForm [

	footnoteIcon := aForm.
]

{ #category : #initialization }
SpVersatileDialogPresenter >> initialize [
	super initialize.

	moreOptionsVisible := false.
	buttons := OrderedDictionary new
]

{ #category : #initialization }
SpVersatileDialogPresenter >> initializeDialogWindow: aDialogWindowPresenter [
	aDialogWindowPresenter initialExtent: 400 @ 200
]

{ #category : #initialization }
SpVersatileDialogPresenter >> initializePresenters [ 

	moreOptionsText := 'More options' asText.
	lessOptionsText := 'Less options' asText.
	moreOptionsButton := self newButton label: moreOptionsText.
	
]

{ #category : #accessing }
SpVersatileDialogPresenter >> layout [
	| mainLayout row |
	mainLayout := SpBoxLayout newVertical.


	"if the content area is not defined, place main message next to the main icon"
	contentArea
		ifNotNil: [ mainMessage
				ifNotNil: [ mainMessagePresenter := (mainMessage isString or: [ mainMessage isText ]) ifTrue: [ self newLabel label: mainMessage ] ifFalse: [ mainMessage ].
					mainLayout add: #mainMessagePresenter ] ].

	row := SpBoxLayout newHorizontal.

	mainIcon
		ifNotNil: [ mainIconPresenter := self newImage image: self mainIcon.
			row add: #mainIconPresenter withConstraints: [ :constraints | constraints width: self mainIcon width * 2 ] ].

	contentArea
		ifNotNil: [ contentAreaPresenter := (contentArea isString or: [ contentArea isText ]) ifTrue: [ self newLabel label: contentArea ] ifFalse: [ contentArea ].
			row add: #contentAreaPresenter ]
		ifNil: [ mainMessage
				ifNotNil: [ mainMessagePresenter := (mainMessage isString or: [ mainMessage isText ]) ifTrue: [ self newLabel label: mainMessage ] ifFalse: [ mainMessage ].
					row add: #mainMessagePresenter ] ].

	mainLayout add: row.

	row := SpBoxLayout newHorizontal.


	moreOptionsArea
		ifNotNil: [ moreOptionsButton label: (moreOptionsVisible ifFalse: [ self moreOptionsText ] ifTrue: [ self lessOptionsText ]).
			moreOptionsButton icon: (self iconNamed: (moreOptionsVisible ifFalse: [ #arrowDown ] ifTrue: [ #arrowUp ])).

			row add: #moreOptionsButton.
			moreOptionsAreaPresenter := (moreOptionsArea isString or: [ moreOptionsArea isText ]) ifTrue: [ self newLabel label: moreOptionsArea ] ifFalse: [ moreOptionsArea ] ].
	buttons keysAndValuesDo: [ :buttonKey :aButton | row add: buttonKey ].

	mainLayout add: row.

	moreOptionsVisible ifTrue: [ mainLayout add: #moreOptionsAreaPresenter ].

	footnoteArea
		ifNotNil: [ row := SpBoxLayout newHorizontal.
			footnoteIcon
				ifNotNil: [ footnoteIconPresenter := self newImage image: footnoteIcon.
					row add: #footnoteIconPresenter withConstraints: [ :constraints | constraints width: self footnoteIcon width * 2 ] ].

			footnoteAreaPresenter := (footnoteArea isString or: [ footnoteArea isText ]) ifTrue: [ self newLabel label: footnoteArea ] ifFalse: [ footnoteArea ].
			row add: #footnoteAreaPresenter ].
	mainLayout add: row.

	defaultButtonId ifNotNil: [ (self buttonAt: defaultButtonId) takeKeyboardFocus ].

	^ mainLayout
]

{ #category : #accessing }
SpVersatileDialogPresenter >> lessOptionsText [

	^ lessOptionsText
]

{ #category : #accessing }
SpVersatileDialogPresenter >> lessOptionsText: aText [

	lessOptionsText := aText.
]

{ #category : #accessing }
SpVersatileDialogPresenter >> mainIcon [

	^ mainIcon
]

{ #category : #accessing }
SpVersatileDialogPresenter >> mainIcon: aFrom [

	mainIcon := aFrom
]

{ #category : #accessing }
SpVersatileDialogPresenter >> mainIconPresenter [
	^ mainIconPresenter
]

{ #category : #accessing }
SpVersatileDialogPresenter >> mainMessage [

	^ mainMessage
]

{ #category : #accessing }
SpVersatileDialogPresenter >> mainMessage: aTextOrPresenter [

	mainMessage := aTextOrPresenter.
]

{ #category : #accessing }
SpVersatileDialogPresenter >> mainMessagePresenter [
	^ mainMessagePresenter
]

{ #category : #accessing }
SpVersatileDialogPresenter >> moreOptionsArea [

	^ moreOptionsArea
]

{ #category : #accessing }
SpVersatileDialogPresenter >> moreOptionsArea: aTextOrPresenter [

	moreOptionsArea := aTextOrPresenter.
]

{ #category : #accessing }
SpVersatileDialogPresenter >> moreOptionsButton [
	^ moreOptionsButton
]

{ #category : #accessing }
SpVersatileDialogPresenter >> moreOptionsText [

	^ moreOptionsText
]

{ #category : #accessing }
SpVersatileDialogPresenter >> moreOptionsText: aText [

	moreOptionsText := aText.
]

{ #category : #accessing }
SpVersatileDialogPresenter >> result [

	^ result
]

{ #category : #accessing }
SpVersatileDialogPresenter >> result: anObject [

	result := anObject
]

{ #category : #private }
SpVersatileDialogPresenter >> return: anObject [

	self result: anObject.
	self window close.
]
