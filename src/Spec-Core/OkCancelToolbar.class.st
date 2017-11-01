"
An OkCancelToolbar is a model for a basic Ok-Cancel toolbar
"
Class {
	#name : #OkCancelToolbar,
	#superclass : #OkToolbar,
	#instVars : [
		'cancelButton',
		'cancelAction',
		'cancelled'
	],
	#category : 'Spec-Core-Support'
}

{ #category : #specs }
OkCancelToolbar class >> defaultSpec [
	<spec>
	
	^ SpecLayout composed
		add: #okButton origin: 1@0 corner: 1@1 offsetOrigin: (2*(self buttonWidth negated))@0 offsetCorner: (self buttonWidth negated)@0;
		add: #cancelButton origin: 1@0 corner: 1@1 offsetOrigin: (self buttonWidth negated)@0 offsetCorner: 0@0;
		send: #vShrinkWrap;
		yourself
]

{ #category : #specs }
OkCancelToolbar class >> title [

	^ 'Ok Cancel'
]

{ #category : #api }
OkCancelToolbar >> cancel [

	self cancelled: true
]

{ #category : #api }
OkCancelToolbar >> cancelAction [

	^ cancelAction value
]

{ #category : #api }
OkCancelToolbar >> cancelAction: aBlock [

	^ cancelAction value: aBlock
]

{ #category : #accessing }
OkCancelToolbar >> cancelButton [

	^ cancelButton
]

{ #category : #api }
OkCancelToolbar >> cancelled [

	^ cancelled value
]

{ #category : #api }
OkCancelToolbar >> cancelled: aBoolean [

	cancelled value: aBoolean
]

{ #category : #initialization }
OkCancelToolbar >> initialize [

	cancelled := false asValueHolder.
	cancelAction := [ true ] asValueHolder.
	super initialize.
]

{ #category : #initialization }
OkCancelToolbar >> initializePresenter [

	super initializePresenter.
	cancelButton action: [ self performCancelAction ]
]

{ #category : #initialization }
OkCancelToolbar >> initializeWidgets [

	super initializeWidgets.
	cancelButton := self newButton.

	self registerCancelButtonEvents.
	self focusOrder
		add: cancelButton
]

{ #category : #api }
OkCancelToolbar >> performCancelAction [

	^ cancelAction value value == false
		ifFalse: [ 
			cancelled value: true.
			owner ifNil: [ self delete ] ifNotNil: [ owner delete ] ]
]

{ #category : #initialization }
OkCancelToolbar >> registerCancelButtonEvents [

	cancelButton 
		label: 'Cancel';
		enabled: true;
		state: false
]

{ #category : #api }
OkCancelToolbar >> triggerCancelAction [

	self performCancelAction
]
