Class {
	#name : #ClyNotebookPageHeaderPresenter,
	#superclass : #SpPresenter,
	#instVars : [
		'pageMorph',
		'iconImage',
		'titleLabel',
		'closeImage',
		'closeAction',
		'doubleClickAction'
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

{ #category : #'event processing' }
ClyNotebookPageHeaderPresenter >> doDoubleClick [

	doubleClickAction ifNil: [ ^ self ].
	doubleClickAction cull: self
]

{ #category : #initialization }
ClyNotebookPageHeaderPresenter >> initializePresenters [

	iconImage := self newImage.
	titleLabel := self newLabel
		addStyle: 'fixed';
		yourself.
	closeImage := self newImage 
		image: (self application iconNamed: #glamorousClose);
		action: [ self doClose ];
		yourself.
	
	self flag: #HACK. "Because Spec2 not yet have a common event handling"
	self whenBuiltDo: [ :anAdapter | 
		titleLabel adapter widget 
			on: #doubleClick send: #doDoubleClick to: self;
			"The following code is required to fix the problem when doubleClick 
			 handler of tab label disables event processing by its parent. 
			 So this is just to bubble up the event"
			on: #mouseDown send: #mouseDown: to: self;
			on: #mouseUp send: #mouseUp: to: self ].
	
	self updatePresenter
]

{ #category : #'event processing' }
ClyNotebookPageHeaderPresenter >> mouseDown: event [ 

	"bubble up"
	self adapter widget owner mouseDown: event
]

{ #category : #'event processing' }
ClyNotebookPageHeaderPresenter >> mouseUp: event [ 

	"bubble up"
	self adapter widget owner mouseUp: event
]

{ #category : #'event handling' }
ClyNotebookPageHeaderPresenter >> onClose: aBlock [ 

	closeAction := aBlock
]

{ #category : #'event handling' }
ClyNotebookPageHeaderPresenter >> onDoubleClick: aBlock [ 

	doubleClickAction := aBlock
]

{ #category : #'accessing model' }
ClyNotebookPageHeaderPresenter >> setModelBeforeInitialization: aPageMorph [

	pageMorph := aPageMorph
]

{ #category : #initialization }
ClyNotebookPageHeaderPresenter >> updatePresenter [

	iconImage image: pageMorph icon.
	titleLabel label: pageMorph label.
	pageMorph isCloseable
		ifTrue: [ closeImage show ]
		ifFalse: [ closeImage hide ]
]
