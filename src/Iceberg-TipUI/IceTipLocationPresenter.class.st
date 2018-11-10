"
I'm an abstract model to provide locations (directories or files). 
I need to be included in other components (search references to my children for examples)
"
Class {
	#name : #IceTipLocationPresenter,
	#superclass : #ComposablePresenter,
	#instVars : [
		'locationLabel',
		'locationInput',
		'chooseButton',
		'label',
		'chooseTitle',
		'chooseBlock',
		'keptLocation'
	],
	#category : #'Iceberg-TipUI-View'
}

{ #category : #accessing }
IceTipLocationPresenter class >> defaultChooseDialogTitle [
	^ 'Choose directory'
]

{ #category : #accessing }
IceTipLocationPresenter class >> defaultLabel [
	^ 'Directory'
]

{ #category : #specs }
IceTipLocationPresenter class >> defaultSpec [ 
	<spec>
	^ SpecLayout composed
		newRow: [ :row | 
			row 
				add: #locationLabel width: self iceLabelWidth; 
			 	add: #locationInput; 
				add: #chooseButton width: self iceIconWidth ]
]

{ #category : #accessing }
IceTipLocationPresenter >> appendPath: aString [
	keptLocation ifNil: [ keptLocation := self location ].
	
	self basicLocation: (aString 
		ifNotEmpty: [ (keptLocation ifNil: [ '.' asFileReference ]) / aString ]
		ifEmpty: [ keptLocation ]).
]

{ #category : #private }
IceTipLocationPresenter >> basicLocation: aReference [
	locationInput text: aReference pathString
]

{ #category : #actions }
IceTipLocationPresenter >> choose [
	self chooseReference ifNotNil: [ :reference | 
		self location: reference.
		"self locationInput text: reference pathString."
		self onChoose ifNotNil: #value ]
]

{ #category : #'accessing ui' }
IceTipLocationPresenter >> chooseButton [
	^ chooseButton
]

{ #category : #'accessing ui' }
IceTipLocationPresenter >> chooseButton: anObject [
	chooseButton := anObject
]

{ #category : #actions }
IceTipLocationPresenter >> chooseReference [ 
	self subclassResponsibility
]

{ #category : #accessing }
IceTipLocationPresenter >> chooseTitle [
	^ chooseTitle ifNil: [ self class defaultChooseDialogTitle ]
]

{ #category : #accessing }
IceTipLocationPresenter >> chooseTitle: aString [
	chooseTitle := aString
]

{ #category : #accessing }
IceTipLocationPresenter >> icon [
	^ self iconNamed: #open
]

{ #category : #initialization }
IceTipLocationPresenter >> initializeWidgets [
	locationLabel := self newLabel label: self label.
	locationInput := self newTextInput autoAccept: true.
	chooseButton := self newButton icon: self icon.
	
	locationInput whenBuiltDo: [ :w | w widget wrapFlag: false ]. 
	chooseButton action: [ self choose ].
	
	self focusOrder add: locationInput
	
]

{ #category : #accessing }
IceTipLocationPresenter >> label [
	^ label ifNil: [ self class defaultLabel ]
]

{ #category : #accessing }
IceTipLocationPresenter >> label: aString [
	label := aString.
	self locationLabel label: aString
	
]

{ #category : #accessing }
IceTipLocationPresenter >> location [
	^ locationInput text trim 
		ifNotEmpty: [ :value | value asFileReference ]
		ifEmpty: [ nil ]
]

{ #category : #accessing }
IceTipLocationPresenter >> location: aReferenceOrString [
	| ref |
	
	ref := aReferenceOrString asFileReference.
	keptLocation ifNotNil: [ 
		| newRef |
		newRef := (self location pathString 
			copyReplaceAll: keptLocation pathString 
			with: ref pathString) 
			asFileReference.
		keptLocation := ref.
		ref := newRef ].
	
	self basicLocation: ref
]

{ #category : #'accessing ui' }
IceTipLocationPresenter >> locationInput [
	^ locationInput
]

{ #category : #'accessing ui' }
IceTipLocationPresenter >> locationInput: anObject [
	locationInput := anObject
]

{ #category : #'accessing ui' }
IceTipLocationPresenter >> locationLabel [
	^ locationLabel
]

{ #category : #'accessing ui' }
IceTipLocationPresenter >> locationLabel: anObject [
	locationLabel := anObject
]

{ #category : #hook }
IceTipLocationPresenter >> onChoose [
	^ chooseBlock
]

{ #category : #hook }
IceTipLocationPresenter >> onChoose: aBlockClosure [
	"I allow to set a hook to execute when the user select a location."

	chooseBlock := aBlockClosure
]

{ #category : #events }
IceTipLocationPresenter >> whenTextChanged: aBlockClosure [ 
	self locationInput whenTextChanged: aBlockClosure
]
