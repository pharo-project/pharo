Class {
	#name : #ClyNotebookPageHeaderPresenter,
	#superclass : #SpPresenter,
	#instVars : [
		'pageMorph',
		'iconImage',
		'titleLabel',
		'closeImage'
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

{ #category : #initialization }
ClyNotebookPageHeaderPresenter >> initializePresenters [

	iconImage := self newImage.
	titleLabel := self newLabel
		addStyle: 'fixed';
		yourself.
	closeImage := self newImage.
	
	self updatePresenter
]

{ #category : #'accessing model' }
ClyNotebookPageHeaderPresenter >> setModelBeforeInitialization: aPageMorph [

	pageMorph := aPageMorph
]

{ #category : #initialization }
ClyNotebookPageHeaderPresenter >> updatePresenter [

	iconImage image: pageMorph icon.
	titleLabel label: pageMorph label.
	closeImage image: (pageMorph isClosable
		ifTrue: [ self iconNamed: #glamorousClose ]
		ifFalse: [ nil ])
]
