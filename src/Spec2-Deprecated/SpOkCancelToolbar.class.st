"
An OkCancelToolbar is a model for a basic Ok-Cancel toolbar
"
Class {
	#name : #SpOkCancelToolbar,
	#superclass : #SpOkToolbar,
	#instVars : [
		'cancelButton',
		'cancelAction',
		'cancelled'
	],
	#category : #'Spec2-Deprecated-Obsolete'
}

{ #category : #specs }
SpOkCancelToolbar class >> defaultSpec [
	<spec>
	
	^ SpLayout composed
		add: #okButton origin: 1@0 corner: 1@1 offsetOrigin: (2*(self buttonWidth negated))@0 offsetCorner: (self buttonWidth negated)@0;
		add: #cancelButton origin: 1@0 corner: 1@1 offsetOrigin: (self buttonWidth negated)@0 offsetCorner: 0@0;
		send: #vShrinkWrap;
		yourself
]

{ #category : #deprecation }
SpOkCancelToolbar class >> isDeprecated [
	"You should use a ToolBarPresenter"

	^ true
]

{ #category : #specs }
SpOkCancelToolbar class >> title [

	^ 'Ok Cancel'
]

{ #category : #api }
SpOkCancelToolbar >> cancel [

	self cancelled: true
]

{ #category : #api }
SpOkCancelToolbar >> cancelAction [

	^ cancelAction value
]

{ #category : #api }
SpOkCancelToolbar >> cancelAction: aBlock [

	^ cancelAction value: aBlock
]

{ #category : #accessing }
SpOkCancelToolbar >> cancelButton [

	^ cancelButton
]

{ #category : #api }
SpOkCancelToolbar >> cancelled [

	^ cancelled value
]

{ #category : #api }
SpOkCancelToolbar >> cancelled: aBoolean [

	cancelled value: aBoolean
]

{ #category : #initialization }
SpOkCancelToolbar >> initialize [

	cancelled := false asValueHolder.
	cancelAction := [ true ] asValueHolder.
	super initialize.
]

{ #category : #initialization }
SpOkCancelToolbar >> initializePresenter [

	super initializePresenter.
	cancelButton action: [ self performCancelAction ]
]

{ #category : #initialization }
SpOkCancelToolbar >> initializeWidgets [

	super initializeWidgets.
	cancelButton := self newButton.

	self registerCancelButtonEvents.
	self focusOrder
		add: cancelButton
]

{ #category : #api }
SpOkCancelToolbar >> performCancelAction [

	^ cancelAction value value == false
		ifFalse: [ 
			cancelled value: true.
			owner ifNil: [ self delete ] ifNotNil: [ owner delete ] ]
]

{ #category : #initialization }
SpOkCancelToolbar >> registerCancelButtonEvents [

	cancelButton 
		label: 'Cancel';
		enabled: true;
		state: false
]

{ #category : #api }
SpOkCancelToolbar >> triggerCancelAction [

	self performCancelAction
]
