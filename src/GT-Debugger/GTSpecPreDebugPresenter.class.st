"
I am a pre debugger showing to the user textual informations about some error.

I'll typically display warning messages. I'll also propose to the user to open a full debugger. 

I'll be open by a debugger if it is needed.
	
Internal Representation and Key Implementation Points.
--------------------

    Instance Variables
	actions:				<aGTSpecPreDebugActionPresenter> 	A presenter holding the actions to execute in the pre debugger.
	debugger:				<aDebugger> 								The debuggeur opening me.
	textPresenter:		<aTextPresenter>						A presenter to display the notification.

"
Class {
	#name : #GTSpecPreDebugPresenter,
	#superclass : #SpPresenter,
	#instVars : [
		'debugger',
		'actions',
		'textPresenter'
	],
	#category : #'GT-Debugger-UI'
}

{ #category : #specs }
GTSpecPreDebugPresenter class >> defaultSpec [
	^ SpBoxLayout newVertical
		add: #actions withConstraints: [ :constraints | constraints height: self toolbarHeight ];
		add: #textPresenter;
		yourself
]

{ #category : #actions }
GTSpecPreDebugPresenter >> close [
	self withWindowDo: #close
]

{ #category : #accessing }
GTSpecPreDebugPresenter >> debugger [
	^ debugger
]

{ #category : #initialization }
GTSpecPreDebugPresenter >> initializeWidgets [
	actions := self instantiate: GTSpecPreDebugActionsPresenter.
	textPresenter := self newText.

	textPresenter disable.

	self focusOrder
		add: actions;
		add: textPresenter
]

{ #category : #initialization }
GTSpecPreDebugPresenter >> initializeWindow: aWindowPresenter [
	aWindowPresenter
		whenClosedDo: [ debugger ifNotNil: #windowIsClosing ];
		initialExtent: 700 @ 180
]

{ #category : #accessing }
GTSpecPreDebugPresenter >> message: aString [
	textPresenter text: aString
]

{ #category : #actions }
GTSpecPreDebugPresenter >> openFullDebugger [
	| currentDebugger |
	currentDebugger := self debugger.
	debugger := nil.
	self close.
	currentDebugger openWithFullView
]

{ #category : #accessing }
GTSpecPreDebugPresenter >> session [
	^ self debugger session
]

{ #category : #'accessing model' }
GTSpecPreDebugPresenter >> setModelBeforeInitialization: aDebugger [
	debugger := aDebugger
]

{ #category : #initialization }
GTSpecPreDebugPresenter >> setTitle: aString [
	self withWindowDo: [ :w | w title: aString ]
]
