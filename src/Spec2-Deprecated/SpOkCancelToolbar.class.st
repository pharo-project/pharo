"
An OkCancelToolbar is a model for a basic Ok-Cancel toolbar
"
Class {
	#name : #SpOkCancelToolbar,
	#superclass : #SpOkToolbar,
	#instVars : [
		'#cancelButton',
		'#cancelAction => SpObservableSlot',
		'#cancelled => SpObservableSlot'
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
	^ cancelAction
]

{ #category : #api }
SpOkCancelToolbar >> cancelAction: aBlock [
	^ cancelAction := aBlock
]

{ #category : #accessing }
SpOkCancelToolbar >> cancelButton [

	^ cancelButton
]

{ #category : #api }
SpOkCancelToolbar >> cancelled [
	^ cancelled
]

{ #category : #api }
SpOkCancelToolbar >> cancelled: aBoolean [
	cancelled := aBoolean
]

{ #category : #initialization }
SpOkCancelToolbar >> connectPresenters [

	super connectPresenters.
	cancelButton action: [ self performCancelAction ]
]

{ #category : #initialization }
SpOkCancelToolbar >> initialize [
	cancelled := false.
	cancelAction := [ true ].
	super initialize
]

{ #category : #initialization }
SpOkCancelToolbar >> initializePresenters [

	super initializePresenters.
	cancelButton := self newButton.

	self registerCancelButtonEvents.
	self focusOrder
		add: cancelButton
]

{ #category : #api }
SpOkCancelToolbar >> performCancelAction [
	^ self cancelAction value == false
		ifFalse: [ cancelled := true.
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
