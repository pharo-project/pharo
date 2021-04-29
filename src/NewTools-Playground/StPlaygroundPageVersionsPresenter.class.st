Class {
	#name : #StPlaygroundPageVersionsPresenter,
	#superclass : #StPresenter,
	#traits : 'SpTModel',
	#classTraits : 'SpTModel classTrait',
	#instVars : [
		'versionList',
		'sourceCode'
	],
	#category : #'NewTools-Playground-View'
}

{ #category : #accessing }
StPlaygroundPageVersionsPresenter class >> preferredExtent [

	^ 700@400
]

{ #category : #actions }
StPlaygroundPageVersionsPresenter >> adoptSelectedVersion [

	self adoptVersion: versionList selectedItem
]

{ #category : #actions }
StPlaygroundPageVersionsPresenter >> adoptVersion: anEntry [

	self parent contents: anEntry content.
	self window close.
	self parent takeKeyboardFocus
]

{ #category : #private }
StPlaygroundPageVersionsPresenter >> displayValueFor: aDateAndTime [

	"^ ((DateAndTime now - aDateAndTime) truncateTo: 2 minutes) humanReadablePrintString"
	^ aDateAndTime epiceaBrowsersAsString
]

{ #category : #actions }
StPlaygroundPageVersionsPresenter >> doAdoptVersion [

	self adoptSelectedVersion
]

{ #category : #actions }
StPlaygroundPageVersionsPresenter >> doAdoptVersionOnNewPlayground [
	| playground |
	
	(playground := StPlayground new) 
		contents: versionList selectedItem content;
		openWithSpec.
	self window close.
	playground takeKeyboardFocus
]

{ #category : #initialization }
StPlaygroundPageVersionsPresenter >> initializeDialogWindow: aDialogWindowPresenter [

	aDialogWindowPresenter
		addButton: 'Adopt' do: [ :presenter | 
			presenter triggerOkAction.
			self adoptSelectedVersion ];
		addButton: 'Close' do: [ :presenter | 
			presenter triggerCancelAction.
			presenter close ]	
]

{ #category : #initialization }
StPlaygroundPageVersionsPresenter >> initializePresenters [

	self layout: (SpPanedLayout newLeftToRight
		positionOfSlider: 40 percent;
		add: (versionList := self newList);
		add: (sourceCode := self newCode);
		yourself).
		
	versionList
		 display: [ :anEntry | self displayValueFor: (anEntry tagAt: #timestamp) ];
		 contextMenu: [ self versionsActions asMenuPresenter ].
	sourceCode 
		beForScripting;
		beNotEditable.
	
	versionList 
		transmitTo: sourceCode 
		transform: [ :anEntry | 
			anEntry 
				ifNotNil: [ anEntry content ]
				ifNil: [ 'no source' ] ]
]

{ #category : #initialization }
StPlaygroundPageVersionsPresenter >> initializeWindow: aWindowPresenter [

	super initializeWindow: aWindowPresenter.
	aWindowPresenter 
		title: 'Versions';
		initialExtent: self class preferredExtent
]

{ #category : #private }
StPlaygroundPageVersionsPresenter >> parent [
	"a synonym to be clearer"

	^ self model
]

{ #category : #initialization }
StPlaygroundPageVersionsPresenter >> updatePresenter [

	versionList items: self model page versions reversed
]

{ #category : #initialization }
StPlaygroundPageVersionsPresenter >> versionsActions [

	^ CmCommandGroup forSpec
		beRoot;
		register: (StPlaygroundAdoptVersionCommand forSpecContext: self);
		register: (StPlaygroundAdoptVersionOnNewPlaygroundCommand forSpecContext: self);
		yourself
]
