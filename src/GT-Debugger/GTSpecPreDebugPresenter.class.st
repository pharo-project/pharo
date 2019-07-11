"
I am an abstract class for pre debug windows.

Depending on the error raised I'll open one of my subclass to display the pre informations to the user.
	
Internal Representation and Key Implementation Points.
--------------------

    Instance Variables
	actions:		<aGTSpecPreDebugActionPresenter> 	A presenter holding the actions to execute in the pre debugger.
	debugger:		<aDebugger> 								The debuggeur opening me.

"
Class {
	#name : #GTSpecPreDebugPresenter,
	#superclass : #SpPresenter,
	#instVars : [
		'debugger',
		'actions'
	],
	#category : #'GT-Debugger-UI'
}

{ #category : #testing }
GTSpecPreDebugPresenter class >> isAbstract [
	^ self = GTSpecPreDebugPresenter
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

	self focusOrder add: actions
]

{ #category : #initialization }
GTSpecPreDebugPresenter >> initializeWindow: aWindowPresenter [
	aWindowPresenter
		whenClosedDo: [ debugger ifNotNil: #windowIsClosing ];
		initialExtent: 700 @ 180
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
