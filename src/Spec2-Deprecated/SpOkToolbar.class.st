"
An OkCancelToolbar is a model for a basic Ok toolbar
"
Class {
	#name : #SpOkToolbar,
	#superclass : #SpPresenter,
	#instVars : [
		'okButton',
		'okAction'
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

{ #category : #private }
SpOkToolbar >> addAll: aWindow  withSpec: aSpec [

	aWindow
		addMorph: (self buildWithSpec: aSpec)
		fullFrame: ((0@0 corner: 1@0) asSpLayoutFrame bottomOffset: 30).
]

{ #category : #api }
SpOkToolbar >> cancel [
	
	owner 
		ifNil: [ self delete ] 
		ifNotNil: [ owner delete ]
]

{ #category : #initialization }
SpOkToolbar >> initialize [

	super initialize.
	okAction := [ true ] asValueHolder.
]

{ #category : #initialization }
SpOkToolbar >> initializePresenter [

	okButton action: self okActionBlock
]

{ #category : #initialization }
SpOkToolbar >> initializeWidgets [
	
	
	okButton := self newButton.
	self registerOkButtonEvents.
	
	self focusOrder
		add: okButton
]

{ #category : #api }
SpOkToolbar >> okAction [

	^ okAction value
]

{ #category : #api }
SpOkToolbar >> okAction: aBlock [

	^ okAction value: aBlock
]

{ #category : #private }
SpOkToolbar >> okActionBlock [

	^ [ okAction value value == false
		ifFalse: [ owner 
			ifNil: [ self delete ] 
			ifNotNil: [ owner delete ] ] ]
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
