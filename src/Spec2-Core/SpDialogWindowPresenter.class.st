"
A DialogWindowPresenter is a presenter used to describe a DialogWindow
"
Class {
	#name : #SpDialogWindowPresenter,
	#superclass : #SpWindowPresenter,
	#instVars : [
		'buttons',
		'okAction',
		'cancelAction',
		'cancelled',
		'defaultButton'
	],
	#category : #'Spec2-Core-Support'
}

{ #category : #specs }
SpDialogWindowPresenter class >> adapterName [

	^ #DialogWindowAdapter
]

{ #category : #private }
SpDialogWindowPresenter >> addButton: aButtonPresenter [

	buttons add: aButtonPresenter.
	^ aButtonPresenter
]

{ #category : #accessing }
SpDialogWindowPresenter >> addButton: aString do: aBlock [
	"Adds button logic to dialog."
	
	^ self addButton: (SpButtonPresenter new 
		owner: self;
		label: aString;
		action: [ aBlock cull: self ];
		yourself)
]

{ #category : #private }
SpDialogWindowPresenter >> addDefaultButton: aButtonPresenter [
	"Adds button logic to dialog."
	
	defaultButton := self addButton: aButtonPresenter.
	defaultButton addStyle: 'default'.
	^ defaultButton
]

{ #category : #accessing }
SpDialogWindowPresenter >> addDefaultButton: aString do: aBlock [
	"Adds button logic to dialog."
	
	defaultButton := self addButton: aString do: aBlock.
	defaultButton addStyle: 'default'.
	^ defaultButton
]

{ #category : #accessing }
SpDialogWindowPresenter >> beCancel [

	cancelled := true
]

{ #category : #accessing }
SpDialogWindowPresenter >> beOk [

	cancelled := false
]

{ #category : #accessing }
SpDialogWindowPresenter >> buttons [ 
	"buttons is an ordered dictionary that matches labels with actions. 
	 Default action will always be the last one."
	
	^ buttons 
]

{ #category : #api }
SpDialogWindowPresenter >> cancelAction [

	^ cancelAction
]

{ #category : #api }
SpDialogWindowPresenter >> cancelAction: aBlock [

	cancelAction := aBlock
]

{ #category : #api }
SpDialogWindowPresenter >> cancelled [

	^ cancelled
]

{ #category : #private }
SpDialogWindowPresenter >> executeDefaultAction [

	defaultButton ifNil: [ ^ self ].
	defaultButton action cull: self
]

{ #category : #initialization }
SpDialogWindowPresenter >> initialize [

	super initialize.
	buttons := OrderedCollection new.
	cancelled := true.
	self initializeDefaultActions
]

{ #category : #initialization }
SpDialogWindowPresenter >> initializeDefaultActions [
	"default ok/cancel actions. 
	 this is just for backwards compatibility"
	self okAction: [ 
		cancelled := false.
		self close ].
	self cancelAction: [ 
		cancelled := true.
		self close ]
]

{ #category : #initialization }
SpDialogWindowPresenter >> initializeWindow [
		
	super initializeWindow.
	self presenter initializeDialogWindow: self
]

{ #category : #testing }
SpDialogWindowPresenter >> isCancelled [

	^ cancelled
]

{ #category : #testing }
SpDialogWindowPresenter >> isDialog [

	^ true
]

{ #category : #testing }
SpDialogWindowPresenter >> isOk [

	^ self isCancelled not
]

{ #category : #api }
SpDialogWindowPresenter >> okAction: aBlock [

	okAction := aBlock
]

{ #category : #api }
SpDialogWindowPresenter >> triggerCancelAction [

	cancelAction ifNil: [ ^ self ].
	cancelAction cull: self
]

{ #category : #api }
SpDialogWindowPresenter >> triggerOkAction [

	okAction ifNil: [ ^ self ].
	okAction cull: self
]
