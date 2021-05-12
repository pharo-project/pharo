"
A simple confirm dialog. 
By default, it will show the `label` message and ask for a conformation with `Yes` and `No` as default button options.

"
Class {
	#name : #SpConfirmDialog,
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
SpConfirmDialog class >> defaultAcceptLabel [

	^ 'Yes'
]

{ #category : #accessing }
SpConfirmDialog class >> defaultCancelLabel [

	^ 'No'
]

{ #category : #accessing }
SpConfirmDialog class >> defaultExtent [

	^ 400@100
]

{ #category : #specs }
SpConfirmDialog class >> defaultSpec [

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
SpConfirmDialog class >> documentFactoryMethodSelector [
	
	^ #newConfirm
]

{ #category : #example }
SpConfirmDialog class >> example [

	^ self new
		title: 'Confirm example';
		label: 'Are you sure?';
		acceptLabel: 'Sure!';
		cancelLabel: 'No, forget it';
		label: 'Are you sure?';
		onAccept: [ self inform: 'Yes!' ];
		onCancel: [ self inform: 'No!' ];
		openDialogWithSpec
]

{ #category : #example }
SpConfirmDialog class >> exampleModal [
	| dialog |

	dialog := self new
		title: 'Confirm modal example';
		label: 'Are you sure?';
		acceptLabel: 'Sure!';
		cancelLabel: 'No, forget it';
		openModalWithSpec.
	
	self inform: (dialog isOk 
		ifTrue: [ 'Yes!' ]
		ifFalse: [ 'No!' ])
]

{ #category : #api }
SpConfirmDialog >> acceptLabel [

	^ acceptLabel ifNil: [ self class defaultAcceptLabel ]
]

{ #category : #api }
SpConfirmDialog >> acceptLabel: aString [

	acceptLabel := aString
]

{ #category : #api }
SpConfirmDialog >> cancelLabel [

	^ cancelLabel ifNil: [ self class defaultCancelLabel ]
]

{ #category : #api }
SpConfirmDialog >> cancelLabel: aString [

	cancelLabel := aString
]

{ #category : #api }
SpConfirmDialog >> extent [

	^ initialExtent ifNil: [ self class defaultExtent ]
]

{ #category : #api }
SpConfirmDialog >> extent: aPoint [

	initialExtent := aPoint
]

{ #category : #initialization }
SpConfirmDialog >> initialize [
	
	super initialize.
	self title: 'Confirm'
]

{ #category : #initialization }
SpConfirmDialog >> initializeDialogWindow: aDialogWindowPresenter [

	super initializeDialogWindow: aDialogWindowPresenter.
	aDialogWindowPresenter
		addButton: self cancelLabel do: [ :presenter | self cancel ];
		addDefaultButton: self acceptLabel do: [ :presenter | self accept ]
		
]

{ #category : #initialization }
SpConfirmDialog >> initializePresenters [

	image := self newImage.
	label := self newLabel
	
]

{ #category : #initialization }
SpConfirmDialog >> initializeWindow: aWindowPresenter [

	aWindowPresenter
		title: self title;
		initialExtent: self extent
]

{ #category : #api }
SpConfirmDialog >> label: aString [

	label label: aString
]

{ #category : #'api-showing' }
SpConfirmDialog >> openDialogWithSpec [
	| dialog |
	
	dialog := super openDialogWithSpec.
	^ dialog isOk
]

{ #category : #api }
SpConfirmDialog >> title [

	^ title
]

{ #category : #api }
SpConfirmDialog >> title: aString [

	title := aString
]
