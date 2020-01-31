"
I am a presenter that will hold the action buttons of a pre debugger.

I'll collect the actions via pragmas and create button presenters for them.
"
Class {
	#name : #GTSpecPreDebugActionsPresenter,
	#superclass : #SpPresenter,
	#traits : 'SpTDynamicPresenter',
	#classTraits : 'SpTDynamicPresenter classTrait',
	#category : #'GT-Debugger-UI'
}

{ #category : #initialization }
GTSpecPreDebugActionsPresenter >> buttonFor: anAction [
	^ (self instantiate: anAction specPresenter)
		debugAction: anAction;
		yourself
]

{ #category : #accessing }
GTSpecPreDebugActionsPresenter >> debugger [
	^ self owner debugger
]

{ #category : #initialization }
GTSpecPreDebugActionsPresenter >> initializeWidgets [
	self preDebuggingActions do: [ :action | self presenterAt: (action id ) asSymbol put: (self buttonFor: action) ].

	self presentersDo: [ :button | self focusOrder add: button ]
]

{ #category : #accessing }
GTSpecPreDebugActionsPresenter >> layout [
	| row |
	row := SpBoxLayout newHorizontal.
	self basicPresenters keysDo: [ :id | row add: id ].
	^ row
]

{ #category : #'actions lookup' }
GTSpecPreDebugActionsPresenter >> preDebuggingActions [
	"Return a collection of debug actions constructed based on the pragmas 'self preDebuggingActionsPragmas'. 
	These actions should understand the message #preDebugWindow:"

	^ (self session class debuggingActionsForPragmas: {#preDebuggingAction} for: self debugger)
		do: [ :each | each preDebugWindow: self owner ]
]

{ #category : #accessing }
GTSpecPreDebugActionsPresenter >> session [
	^ self owner session
]
