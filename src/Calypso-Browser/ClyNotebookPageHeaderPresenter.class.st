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

	^ SpBoxLayout newLeftToRight
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
		yourself.
		
	closeImage eventHandler 
		whenMouseDownDo: [ :anEvent | 
			anEvent isPrimaryButton 
				ifTrue: [ self doClose ] ].

	titleLabel eventHandler
		"This is required to fix a problem that arrives when doubleClick 
		 event of tab label happens: ot disables event processing by its parent. 
		 So this is just to bubble up the event"
		whenMouseDownDo: [ :anEvent | self mouseDown: anEvent ];
		whenMouseUpDo: [ :anEvent | self mouseUp: anEvent ];
		whenDoubleClickDo: [ :anEvent | self doDoubleClick ].

	self updatePresenter
]

{ #category : #'event processing' }
ClyNotebookPageHeaderPresenter >> mouseDown: event [ 

	"bubble up"
	self adapter widget owner eventHandler 
		mouseDown: event 
		fromMorph: self adapter widget owner
]

{ #category : #'event processing' }
ClyNotebookPageHeaderPresenter >> mouseUp: event [ 

	"bubble up"
	self adapter widget owner eventHandler 
		mouseUp: event 
		fromMorph: self adapter widget owner
]

{ #category : #'event handling' }
ClyNotebookPageHeaderPresenter >> onClose: aBlock [ 

	closeAction := aBlock
]

{ #category : #'event handling' }
ClyNotebookPageHeaderPresenter >> onDoubleClick: aBlock [ 

	doubleClickAction := aBlock
]

{ #category : #'accessing - model' }
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
