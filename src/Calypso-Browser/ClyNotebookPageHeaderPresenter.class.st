Class {
	#name : #ClyNotebookPageHeaderPresenter,
	#superclass : #SpPresenter,
	#instVars : [
		'pageMorph',
		'iconImage',
		'titleLabel',
		'closeImage',
		'closeAction'
	],
	#category : #'Calypso-Browser-Tabs'
}

{ #category : #specs }
ClyNotebookPageHeaderPresenter class >> defaultSpec [

	^ SpBoxLayout newHorizontal
		spacing: 3;
		add: #iconImage expand: false;
		add: #titleLabel;
		addLast: #closeImage expand: false;
		yourself
]

{ #category : #'event processing' }
ClyNotebookPageHeaderPresenter >> doClose [

	closeAction ifNil: [ ^ self ].
	closeAction cull: self
]

{ #category : #initialization }
ClyNotebookPageHeaderPresenter >> initializePresenters [

	iconImage := self newImage.
	titleLabel := self newLabel
		addStyle: 'fixed';
		yourself.
	closeImage := self newImage.
	
	self updatePresenter
]

{ #category : #'event handling' }
ClyNotebookPageHeaderPresenter >> onClose: aBlock [ 

	closeAction := aBlock
]

{ #category : #'accessing model' }
ClyNotebookPageHeaderPresenter >> setModelBeforeInitialization: aPageMorph [

	pageMorph := aPageMorph
]

{ #category : #initialization }
ClyNotebookPageHeaderPresenter >> updatePresenter [

	iconImage image: pageMorph icon.
	titleLabel label: pageMorph label.
	pageMorph isClosable
		ifTrue: [ closeImage show ]
		ifFalse: [ closeImage hide ]
]
