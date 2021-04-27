"
A simple dialog to request some information (a text)
"
Class {
	#name : #SpRequestDialog,
	#superclass : #SpDialogPresenter,
	#instVars : [
		'label',
		'textInput',
		'title',
		'initialExtent',
		'acceptLabel',
		'cancelLabel'
	],
	#category : #'Spec2-Dialogs'
}

{ #category : #accessing }
SpRequestDialog class >> defaultExtent [

	^ 400@140
]

{ #category : #specs }
SpRequestDialog class >> defaultSpec [

	^ SpBoxLayout newVertical
		borderWidth: 5;
		spacing: 5;
		add: #label expand: false;
		add: #textInput expand: false;
		yourself
]

{ #category : #documentation }
SpRequestDialog class >> documentFactoryMethodSelector [
	
	^ #newRequest
]

{ #category : #example }
SpRequestDialog class >> example [

	self new
		title: 'Request example';
		label: 'The meaning of life?';
		text: 'I am tempted to say 42...';
		acceptLabel: 'I know!';
		cancelLabel: 'Cancel';
		onAccept: [ :dialog | self inform: dialog presenter text ];
		openDialogWithSpec
]

{ #category : #example }
SpRequestDialog class >> exampleModal [
	| dialog |

	dialog := self new
		title: 'Request modal example';
		label: 'The meaning of life?';
		text: 'I am tempted to say 42...';
		acceptLabel: 'I know!';
		cancelLabel: 'Cancel';
		openModalWithSpec.
		
	dialog isOk ifTrue: [ 
		self inform: dialog presenter text ]
]

{ #category : #api }
SpRequestDialog >> acceptLabel [

	^ acceptLabel ifNil: [ self class defaultAcceptLabel ]
]

{ #category : #api }
SpRequestDialog >> acceptLabel: aString [

	acceptLabel := aString
]

{ #category : #api }
SpRequestDialog >> cancelLabel [

	^ cancelLabel ifNil: [ self class defaultCancelLabel ]
]

{ #category : #api }
SpRequestDialog >> cancelLabel: aString [

	cancelLabel := aString
]

{ #category : #api }
SpRequestDialog >> extent [

	^ initialExtent ifNil: [ self class defaultExtent ]
]

{ #category : #api }
SpRequestDialog >> extent: aPoint [

	initialExtent := aPoint
]

{ #category : #initialization }
SpRequestDialog >> initialize [
	
	super initialize.
	self title: 'Request'
]

{ #category : #initialization }
SpRequestDialog >> initializeDialogWindow: aDialogWindowPresenter [

	super initializeDialogWindow: aDialogWindowPresenter.
	aDialogWindowPresenter
		addButton: self cancelLabel do: [ :presenter | self cancel ];
		addDefaultButton: self acceptLabel do: [ :presenter | self accept ]
]

{ #category : #initialization }
SpRequestDialog >> initializePresenters [

	label := self newLabel.
	textInput := self newTextInput
]

{ #category : #initialization }
SpRequestDialog >> initializeWindow: aWindowPresenter [

	aWindowPresenter
		title: self title;
		initialExtent: self extent;
		whenOpenedDo: [ textInput takeKeyboardFocus ]
]

{ #category : #api }
SpRequestDialog >> label: aString [

	label label: aString
]

{ #category : #'api-showing' }
SpRequestDialog >> openDialogWithSpec [
	| dialog |
	
	dialog := super openDialogWithSpec.
	^ dialog isOk
		ifTrue: [ self text trimmed ]
		ifFalse: [ nil ]
]

{ #category : #api }
SpRequestDialog >> text [ 
	
	^ textInput text trimmed
]

{ #category : #api }
SpRequestDialog >> text: aString [

	textInput text: aString
]

{ #category : #api }
SpRequestDialog >> title [

	^ title
]

{ #category : #api }
SpRequestDialog >> title: aString [

	title := aString
]
