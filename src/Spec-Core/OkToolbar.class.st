"
An OkCancelToolbar is a model for a basic Ok toolbar
"
Class {
	#name : #OkToolbar,
	#superclass : #ComposablePresenter,
	#instVars : [
		'okButton',
		'okAction'
	],
	#category : #Spec-Core-Support
}

{ #category : #specs }
OkToolbar class >> defaultSpec [
	<spec>
	
	^ SpecLayout composed
		add: #okButton origin: 1@0 corner: 1@1 offsetOrigin: (self buttonWidth negated)@0 offsetCorner: 0@0;
		send: #vShrinkWrap;
		yourself
]

{ #category : #specs }
OkToolbar class >> title [

	^ 'Ok Toolbar'
]

{ #category : #private }
OkToolbar >> addAll: aWindow  withSpec: aSpec [

	aWindow
		addMorph: (self buildWithSpec: aSpec)
		fullFrame: ((0@0 corner: 1@0) asSpecLayoutFrame bottomOffset: 30).
]

{ #category : #api }
OkToolbar >> cancel [
	
	owner 
		ifNil: [ self delete ] 
		ifNotNil: [ owner delete ]
]

{ #category : #initialization }
OkToolbar >> initialize [

	super initialize.
	okAction := [ true ] asValueHolder.
]

{ #category : #initialization }
OkToolbar >> initializePresenter [

	okButton action: self okActionBlock
]

{ #category : #initialization }
OkToolbar >> initializeWidgets [
	
	
	okButton := self newButton.
	self registerOkButtonEvents.
	
	self focusOrder
		add: okButton
]

{ #category : #api }
OkToolbar >> okAction [

	^ okAction value
]

{ #category : #api }
OkToolbar >> okAction: aBlock [

	^ okAction value: aBlock
]

{ #category : #private }
OkToolbar >> okActionBlock [

	^ [ okAction value value == false
		ifFalse: [ owner 
			ifNil: [ self delete ] 
			ifNotNil: [ owner delete ] ] ]
]

{ #category : #accessing }
OkToolbar >> okButton [

	^ okButton
]

{ #category : #initialization }
OkToolbar >> registerOkButtonEvents [

	okButton 
		label: 'Ok';
		enabled: true;
		state: false
]

{ #category : #api }
OkToolbar >> triggerCancelAction [
	
	self cancel
]

{ #category : #api }
OkToolbar >> triggerOkAction [

	self okActionBlock value
]
