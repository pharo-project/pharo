"
A dialog to present some simple information to users.
"
Class {
	#name : #SpInformDialog,
	#superclass : #SpDialogPresenter,
	#instVars : [
		'label',
		'image',
		'title',
		'initialExtent',
		'acceptLabel',
		'cancelLabel'
	],
	#category : #'Spec2-Dialogs'
}

{ #category : #accessing }
SpInformDialog class >> defaultAcceptLabel [

	^ 'Ok'
]

{ #category : #accessing }
SpInformDialog class >> defaultExtent [

	^ 400@100
]

{ #category : #specs }
SpInformDialog class >> defaultSpec [

	^ SpBoxLayout newVertical
		borderWidth: 5;
		spacing: 5;
		add: (SpBoxLayout newHorizontal
			spacing: 5;
			add: #image expand: false;
			add: #label;
			yourself);
		yourself
]

{ #category : #documentation }
SpInformDialog class >> documentFactoryMethodSelector [
	
	^ #newInform
]

{ #category : #example }
SpInformDialog class >> example [

	self new
		title: 'Inform example';
		label: 'You are seeing an inform dialog!';
		acceptLabel: 'Close this!';
		openDialogWithSpec
]

{ #category : #example }
SpInformDialog class >> exampleModal [

	self new
		title: 'Inform modal example';
		label: 'You are seeing an inform dialog!';
		acceptLabel: 'Close this!';
		openModalWithSpec
]

{ #category : #api }
SpInformDialog >> acceptLabel [

	^ acceptLabel ifNil: [ self class defaultAcceptLabel ]
]

{ #category : #api }
SpInformDialog >> acceptLabel: aString [

	acceptLabel := aString
]

{ #category : #api }
SpInformDialog >> extent [

	^ initialExtent ifNil: [ self class defaultExtent ]
]

{ #category : #api }
SpInformDialog >> extent: aPoint [

	initialExtent := aPoint
]

{ #category : #initialization }
SpInformDialog >> initialize [

	super initialize.
	self title: 'Inform'
]

{ #category : #initialization }
SpInformDialog >> initializeDialogWindow: aDialogWindowPresenter [

	super initializeDialogWindow: aDialogWindowPresenter.
	aDialogWindowPresenter
		addDefaultButton: self acceptLabel 
		do: [ :presenter | self accept ]
		
]

{ #category : #initialization }
SpInformDialog >> initializePresenters [

	image := self newImage.
	label := self newLabel
]

{ #category : #initialization }
SpInformDialog >> initializeWindow: aWindowPresenter [

	aWindowPresenter
		title: self title;
		initialExtent: self extent
]

{ #category : #api }
SpInformDialog >> label: aString [

	label label: aString
]

{ #category : #api }
SpInformDialog >> title [

	^ title
]

{ #category : #api }
SpInformDialog >> title: aString [

	title := aString
]
