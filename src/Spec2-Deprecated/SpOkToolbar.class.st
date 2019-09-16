"
An OkCancelToolbar is a model for a basic Ok toolbar
"
Class {
	#name : #SpOkToolbar,
	#superclass : #SpPresenter,
	#instVars : [
		'#okButton',
		'#okAction => SpObservableSlot'
	],
	#category : #'Spec2-Deprecated-Obsolete'
}

{ #category : #specs }
SpOkToolbar class >> defaultSpec [
	<spec>
	
	^ SpLayout composed
		add: #okButton origin: 1@0 corner: 1@1 offsetOrigin: (self buttonWidth negated)@0 offsetCorner: 0@0;
		send: #vShrinkWrap;
		yourself
]

{ #category : #deprecation }
SpOkToolbar class >> isDeprecated [
	"You should use a ToolBarPresenter"

	^ true
]

{ #category : #specs }
SpOkToolbar class >> title [

	^ 'Ok Toolbar'
]

{ #category : #api }
SpOkToolbar >> cancel [
	
	owner 
		ifNil: [ self delete ] 
		ifNotNil: [ owner delete ]
]

{ #category : #initialization }
SpOkToolbar >> connectPresenters [

	okButton action: self okActionBlock
]

{ #category : #initialization }
SpOkToolbar >> initialize [
	super initialize.
	okAction := [ true ]
]

{ #category : #initialization }
SpOkToolbar >> initializePresenters [
	
	
	okButton := self newButton.
	self registerOkButtonEvents.
	
	self focusOrder
		add: okButton
]

{ #category : #api }
SpOkToolbar >> okAction [
	^ okAction
]

{ #category : #api }
SpOkToolbar >> okAction: aBlock [

	^ okAction := aBlock
]

{ #category : #private }
SpOkToolbar >> okActionBlock [
	^ [ self okAction value == false ifFalse: [ owner ifNil: [ self delete ] ifNotNil: [ owner delete ] ] ]
]

{ #category : #accessing }
SpOkToolbar >> okButton [

	^ okButton
]

{ #category : #initialization }
SpOkToolbar >> registerOkButtonEvents [

	okButton 
		label: 'Ok';
		enabled: true;
		state: false
]

{ #category : #api }
SpOkToolbar >> triggerCancelAction [
	
	self cancel
]

{ #category : #api }
SpOkToolbar >> triggerOkAction [

	self okActionBlock value
]
