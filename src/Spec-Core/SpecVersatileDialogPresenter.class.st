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
	#name : #SpecVersatileDialogPresenter,
	#superclass : #DynamicComposablePresenter,
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
	#category : #'Spec-Core-Widgets'
}

{ #category : #specs }
SpecVersatileDialogPresenter class >> example1 [ 

	<example>
	
	| dialog |
	
	dialog := SpecVersatileDialogPresenter new.
	dialog title: 'Confirmation'.
	dialog mainMessage: 'Confirm your choice' asText allBold.
	dialog mainIcon: (self iconNamed: #question).
	dialog contentArea: 'Do you want to discard your content?'.
	dialog moreOptionsArea: (dialog newCheckBox label: 'Discard permanently').
	dialog addButton: #yes text: 'yes' value: #yes.
	dialog addButton: #no text: 'no' value: #no.
	dialog addButton: #cancel text: 'cancel' value: #cancel.
	dialog footnoteIcon: (self iconNamed: #error).
	dialog footnoteArea: 'This operation can be very dangerous.'.
	dialog openModalWithSpec.
	dialog result logCr.

	self inform: (dialog moreOptionsArea state asString).


]

{ #category : #specs }
SpecVersatileDialogPresenter class >> example2 [

	<example>
	
	| dialog |
	
	dialog := SpecVersatileDialogPresenter new.
	dialog title: 'Confirmation'.
	dialog mainMessage: 'Save content' asText allBold.
	dialog addButton: #save text: 'Save' value: true.
	dialog addButton: #cancel text: 'Cancel' value: false.
	dialog mainIcon: (self iconNamed: #question).
	
	dialog openModalWithSpec.

	dialog result logCr.




]

{ #category : #specs }
SpecVersatileDialogPresenter class >> example3 [

	<example>
	
	| dialog |
	
	dialog := SpecVersatileDialogPresenter new.
	dialog title: 'Confirmation'.
	dialog mainMessage: 'Save content' asText allBold.
	dialog addButton: #save text: 'Save' value: true.
	dialog addButton: #cancel text: 'Cancel' value: false.
	dialog mainIcon: (self iconNamed: #question).
	
	(dialog buttonAt: #save) icon: (self iconNamed: #smallSave).
	dialog footnoteIcon: (self iconNamed: #error).
	dialog footnoteArea: 'Saving may take several minutes'.
	
	dialog openModalWithSpec.
	dialog result logCr.

	



]

{ #category : #specs }
SpecVersatileDialogPresenter class >> example4 [

	<example>
	
	| dialog |
	
	dialog := SpecVersatileDialogPresenter new.
	dialog title: 'Select mumber'.
	1 to: 5 do: [ :aNumber |
		dialog addButton: ('button', aNumber asString) asSymbol 
			text: aNumber printStringRoman 
			value: aNumber ].
	
	dialog defaultButtonId: #button2.
	
	dialog openModalWithSpec.
	dialog result logCr.

	



]

{ #category : #specs }
SpecVersatileDialogPresenter class >> example5 [

	<example>
	
	| dialog listPresenter |
	
	dialog := SpecVersatileDialogPresenter new.
	dialog title: 'Confirmation'.
	listPresenter := dialog newList.
	listPresenter items: #(one two three).
	dialog contentArea: listPresenter.
	dialog addButton: #ok text: 'OK' value: true condition: [ listPresenter selection isEmpty not ].
	dialog addButton: #cancel text: 'Cancel' value: false.
	dialog mainIcon: (self iconNamed: #question).
	
	dialog openModalWithSpec.

	dialog result logCr.




]

{ #category : #api }
SpecVersatileDialogPresenter >> addButton: id text: buttonText value: anObject [

	^ self addButton: id text: buttonText value: anObject condition: true
]

{ #category : #api }
SpecVersatileDialogPresenter >> addButton: id text: buttonText value: anObject condition: aCondition [

	| aButton |
	
	aButton := self newButton label: buttonText.
	aButton action: [ 
		aCondition value ifTrue: [ 
			self return: anObject ] ].

	buttons at: id put: aButton.

	self additionalSubpresentersMap at: id put: aButton.
	
]

{ #category : #api }
SpecVersatileDialogPresenter >> buttonAt: id [

	^ buttons at: id
]

{ #category : #accessing }
SpecVersatileDialogPresenter >> buttons [

	^ buttons
]

{ #category : #accessing }
SpecVersatileDialogPresenter >> contentArea [

	^ contentArea
]

{ #category : #accessing }
SpecVersatileDialogPresenter >> contentArea: aTextOrPresenter [

	contentArea := aTextOrPresenter.
]

{ #category : #accessing }
SpecVersatileDialogPresenter >> contentAreaPresenter [
	^ contentAreaPresenter
]

{ #category : #accessing }
SpecVersatileDialogPresenter >> defaultButtonId [
	^ defaultButtonId
]

{ #category : #accessing }
SpecVersatileDialogPresenter >> defaultButtonId: anObject [
	defaultButtonId := anObject
]

{ #category : #accessing }
SpecVersatileDialogPresenter >> footnoteArea [

	^ footnoteArea
]

{ #category : #accessing }
SpecVersatileDialogPresenter >> footnoteArea: aTextOrPresenter [

	footnoteArea := aTextOrPresenter.
]

{ #category : #accessing }
SpecVersatileDialogPresenter >> footnoteIcon [

	^ footnoteIcon
]

{ #category : #accessing }
SpecVersatileDialogPresenter >> footnoteIcon: aForm [

	footnoteIcon := aForm.
]

{ #category : #'api-window' }
SpecVersatileDialogPresenter >> initialExtent [
	^ 400@200
]

{ #category : #initialization }
SpecVersatileDialogPresenter >> initialize [

	super initialize.
	
	moreOptionsVisible := false.
	buttons := OrderedDictionary new.
	result := nil.

]

{ #category : #initialization }
SpecVersatileDialogPresenter >> initializeDialogWindow: aDialogWindowPresenter [
]

{ #category : #initialization }
SpecVersatileDialogPresenter >> initializePresenter [ 

	super initializePresenter.
	
	moreOptionsButton action: [ 
		moreOptionsVisible := moreOptionsVisible not.
		self needRebuild: false.
		self buildWithSpec ].
	

]

{ #category : #initialization }
SpecVersatileDialogPresenter >> initializeWidgets [ 

	moreOptionsText := 'More options' asText.
	lessOptionsText := 'Less options' asText.
	moreOptionsButton := self newButton label: moreOptionsText.
	
]

{ #category : #accessing }
SpecVersatileDialogPresenter >> layout [

	| mainLayout |
	
	mainLayout := SpecLayout composed.
	
	mainLayout newColumn: [ :column | 

		"if the content area is not defined, place main message next to the main icon"
		contentArea ifNotNil: [ 
			mainMessage ifNotNil: [
				mainMessagePresenter := (mainMessage isString or: [ mainMessage isText ]) 
					ifTrue: [ self newLabel label: mainMessage ]
					ifFalse: [ mainMessage ].
				column add: #mainMessagePresenter ] ].
		
		column newRow: [ :row |		
		mainIcon ifNotNil: [ 
			mainIconPresenter := self newImage image: self mainIcon.
			row add: #mainIconPresenter width: (self mainIcon width * 2) ]	.
		
		contentArea 
			ifNotNil: [
				contentAreaPresenter := (contentArea isString or: [ contentArea isText ]) 
					ifTrue: [ self newLabel label: contentArea ]
					ifFalse: [ contentArea ].
				row add: #contentAreaPresenter ]
			ifNil: [ 
				mainMessage ifNotNil: [
				mainMessagePresenter := (mainMessage isString or: [ mainMessage isText ]) 
					ifTrue: [ self newLabel label: mainMessage ]
					ifFalse: [ mainMessage ].
				row add: #mainMessagePresenter ]  ] ].

		column newRow: [ :row |	
			moreOptionsArea ifNotNil: [
				moreOptionsButton label: (moreOptionsVisible
					ifFalse: [ self moreOptionsText ]
					ifTrue: [ self lessOptionsText ]).
				moreOptionsButton icon: (moreOptionsVisible
					ifFalse: [ self iconNamed: #arrowDown ]
					ifTrue: [ self iconNamed: #arrowUp ]).
				
				row add: #moreOptionsButton.
				moreOptionsAreaPresenter := (moreOptionsArea isString or: [ moreOptionsArea isText ]) 
					ifTrue: [ self newLabel label: moreOptionsArea ]
					ifFalse: [ moreOptionsArea ]] .	
			buttons keysAndValuesDo: [ :buttonKey :aButton |
				row add: buttonKey ]].
		
		moreOptionsVisible ifTrue: [ 
			column add: #moreOptionsAreaPresenter ].
		
		footnoteArea ifNotNil: [
			column newRow: [ :row |	
				footnoteIcon ifNotNil: [ 
					footnoteIconPresenter := self newImage image: footnoteIcon.
					row add: #footnoteIconPresenter width: (self footnoteIcon width * 2) ]	.
			
					footnoteAreaPresenter := (footnoteArea isString or: [ footnoteArea isText ]) 
						ifTrue: [ self newLabel label: footnoteArea ]
						ifFalse: [ footnoteArea ].
					row add: #footnoteAreaPresenter ] ] ].
		
		defaultButtonId ifNotNil: [ 
			(self buttonAt: defaultButtonId) takeKeyboardFocus ].
	
	^ mainLayout
]

{ #category : #accessing }
SpecVersatileDialogPresenter >> lessOptionsText [

	^ lessOptionsText
]

{ #category : #accessing }
SpecVersatileDialogPresenter >> lessOptionsText: aText [

	lessOptionsText := aText.
]

{ #category : #accessing }
SpecVersatileDialogPresenter >> mainIcon [

	^ mainIcon
]

{ #category : #accessing }
SpecVersatileDialogPresenter >> mainIcon: aFrom [

	mainIcon := aFrom
]

{ #category : #accessing }
SpecVersatileDialogPresenter >> mainIconPresenter [
	^ mainIconPresenter
]

{ #category : #accessing }
SpecVersatileDialogPresenter >> mainMessage [

	^ mainMessage
]

{ #category : #accessing }
SpecVersatileDialogPresenter >> mainMessage: aTextOrPresenter [

	mainMessage := aTextOrPresenter.
]

{ #category : #accessing }
SpecVersatileDialogPresenter >> mainMessagePresenter [
	^ mainMessagePresenter
]

{ #category : #accessing }
SpecVersatileDialogPresenter >> moreOptionsArea [

	^ moreOptionsArea
]

{ #category : #accessing }
SpecVersatileDialogPresenter >> moreOptionsArea: aTextOrPresenter [

	moreOptionsArea := aTextOrPresenter.
]

{ #category : #accessing }
SpecVersatileDialogPresenter >> moreOptionsButton [
	^ moreOptionsButton
]

{ #category : #accessing }
SpecVersatileDialogPresenter >> moreOptionsText [

	^ moreOptionsText
]

{ #category : #accessing }
SpecVersatileDialogPresenter >> moreOptionsText: aText [

	moreOptionsText := aText.
]

{ #category : #accessing }
SpecVersatileDialogPresenter >> presenterNamed: aString [

	^ self additionalSubpresentersMap at: aString
]

{ #category : #accessing }
SpecVersatileDialogPresenter >> result [

	^ result
]

{ #category : #accessing }
SpecVersatileDialogPresenter >> result: anObject [

	result := anObject
]

{ #category : #private }
SpecVersatileDialogPresenter >> return: anObject [

	self result: anObject.
	self window close.
]
