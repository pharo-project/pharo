"
A DialogWindowPresenter is a presenter used to describe a DialogWindow
"
Class {
	#name : #DialogWindowPresenter,
	#superclass : #WindowPresenter,
	#instVars : [
		'buttons',
		'okAction',
		'cancelAction',
		'cancelled'
	],
	#category : #'Spec-Core-Support'
}

{ #category : #specs }
DialogWindowPresenter class >> adapterName [

	^ #DialogWindowAdapter
]

{ #category : #accessing }
DialogWindowPresenter >> addButton: aString do: aBlock [
	"Adds button logic to dialog."
	
	^ buttons add: (ButtonPresenter new 
		owner: self;
		label: aString;
		action: [ aBlock cull: self ];
		yourself)
]

{ #category : #accessing }
DialogWindowPresenter >> buttons [ 
	"buttons is an ordered dictionary that matches labels with actions. 
	 Default action will always be the last one."
	
	^ buttons 
]

{ #category : #api }
DialogWindowPresenter >> cancelAction [

	^ cancelAction
]

{ #category : #api }
DialogWindowPresenter >> cancelAction: aBlock [

	cancelAction := aBlock
]

{ #category : #api }
DialogWindowPresenter >> cancelled [

	^ cancelled
]

{ #category : #initialization }
DialogWindowPresenter >> initialize [

	super initialize.
	buttons := OrderedCollection new.
	self initializeDefaultActions
]

{ #category : #initialization }
DialogWindowPresenter >> initializeDefaultActions [
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
DialogWindowPresenter >> initializeWindow [
		
	super initializeWindow.
	self presenter initializeDialogWindow: self
]

{ #category : #api }
DialogWindowPresenter >> okAction: aBlock [

	okAction := aBlock
]

{ #category : #api }
DialogWindowPresenter >> triggerCancelAction [

	cancelAction ifNil: [ ^ self ].
	cancelAction cull: self
]

{ #category : #api }
DialogWindowPresenter >> triggerOkAction [

	okAction ifNil: [ ^ self ].
	okAction cull: self
]
